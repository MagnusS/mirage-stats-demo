open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (S:Cohttp_lwt.Server) (Clock : V1.CLOCK)= struct
  let finish_boot_ts = ref 0.0

  let () =
    finish_boot_ts := (Clock.time ());
    Printf.printf "Current time is %f\n%!" (!finish_boot_ts)

  let start c http clock =
    C.log_s c "Starting ....\n" >>= fun () ->

    (* get start time from xen *)
    let start_time =
      OS.Xs.make () >>= fun client ->
      OS.Xs.(immediate client (fun x -> read x "vm")) >>= fun vm ->
      OS.Xs.(immediate client (fun x -> read x (vm^"/start_time"))) >|= fun start_time ->
      (* TODO HACK strtod not implemented in minios, so can't use float_of_string without segfault *)
      let a = Str.split (Str.regexp "[.]+") start_time in
      let v = int_of_string (List.hd a) in 
      let d_str = (List.hd (List.tl a)) in
      Printf.printf "%s %s (%s)\n" (List.hd a) d_str start_time;
      let d = int_of_string d_str in
      assert ((String.length d_str) == 2); 
      (float_of_int v) +. ((float_of_int d) /. 100.0)
      (* end of HACK *)
    in

    (* Split a URI into a list of path segments *)
    let split_path uri =
      let path = Uri.path uri in
      let rec aux = function
        | [] | [""] -> []
        | hd::tl -> hd :: aux tl
      in
      List.filter (fun e -> e <> "")
        (aux (Re_str.(split_delim (regexp_string "/") path)))
    in

    (* dispatch non-file URLs *)
    let rec dispatcher = function
      | [] | [""] -> dispatcher ["index.html"] 
      | segments ->
        let on_time = Clock.time ()  in
        start_time >>= fun st ->
        let body = (Printf.sprintf "<html>\n<body>\n<h1>Hello World from Jitsu!</h1><br />Unikernel booted in %f seconds, %f seconds ago\n</body>\n</html>" 
                      ((!finish_boot_ts) -. st) 
                      (on_time -. st)) in
        S.respond_string ~status:`OK ~body ()
    in

    (* HTTP callback *)
    let callback conn_id request body =
      let uri = S.Request.uri request in
      dispatcher (split_path uri)
    in
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in
    http (S.make ~conn_closed ~callback ())

end
