open Lwt
open Printf
open V1_LWT

let pb = Printf.bprintf

let time ~boot ~ago =
  Printf.sprintf
    "<div class=\"time\">\n\
     Unikernel booted in %f seconds, %f seconds ago\n\
     </div>" boot ago

let manifest () =
  let open Opam_manifest in
  let buf = Buffer.create 1024 in
  pb buf "<div class=\"manifest\">\n\
          <h3>Manifest (%d packages)</h3>\n\
          <ul>\n" (List.length Opam_manifest.all);
  List.iter (fun pkg ->
      match pkg.archive with
      | "" -> pb buf "<li>%s.%s</li>\n" pkg.name pkg.version
      | a  -> pb buf "<li><a href=%S>%s.%s</a></li>\n" a pkg.name pkg.version
    ) Opam_manifest.all;
  pb buf "</ul>\n\
          </div>";
  Buffer.contents buf

let gc () =
  let open Gc in
  let k f = Printf.sprintf "%dk" (f / 1_000) in
  let m f = Printf.sprintf "%.0fm" (f /. 1_000_000.) in
  let t = Gc.stat () in
  Printf.sprintf
    "<div class=\"gc\">\n\
     <ul>\n\
     <li>%s</li>\n\
     <li>%s</li>\n\
     <li>%s</li>\n\
     </ul>\n\
     </div>"
    (m (Gc.allocated_bytes ()))
    (k t.heap_words)
    (k t.live_words)

let body ~boot ~ago =
  Printf.sprintf
    "<html>\n\
     <head>\n\
     <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">\n\
     </head>\n\
     <body>\n\
     <h1>Hello World from Jitsu!</h1>\
     <hr />\
     <div class=\"internals\">\n\
     %s\n\
     %s\n\
     %s\n\
     </div>\n\
     </body>\n\
     </html>"
    (time ~boot ~ago) (gc ()) (manifest ())

let style_css =
  ".internals {\n\
  \  background-color: black;\n\
  \  color: white;\n\
   \ width: 100%;\n\
   }"

(* Split a URI into a list of path segments *)
let split_path uri =
  let path = Uri.path uri in
  Re_str.(split_delim (regexp_string "/") path)
  |> List.filter (fun e -> e <> "")

module Main (C:CONSOLE) (S:Cohttp_lwt.Server) (Clock : V1.CLOCK)= struct

  let finish_boot_ts = ref 0.0

  let () =
    finish_boot_ts := (Clock.time ());
    Printf.printf "Current time is %f\n%!" (!finish_boot_ts)

  (* get start time from xen *)
  let start_time () =
    OS.Xs.make () >>= fun client ->
    OS.Xs.(immediate client (fun x -> read x "vm")) >>= fun vm ->
    OS.Xs.(immediate client (fun x -> read x (vm^"/start_time")))
    >|= fun start_time ->
    (* TODO HACK strtod not implemented in minios, so can't use
       float_of_string without segfault *)
    let a = Str.split (Str.regexp "[.]+") start_time in
    let v = int_of_string (List.hd a) in
    let d_str = (List.hd (List.tl a)) in
    Printf.printf "%s %s (%s)\n" (List.hd a) d_str start_time;
    let d = int_of_string d_str in
    assert ((String.length d_str) == 2);
    (float_of_int v) +. ((float_of_int d) /. 100.0)
    (* end of HACK *)

  (* dispatch non-file URLs *)
  let rec dispatcher = function
    | [] -> dispatcher ["index.html"]
    | ["style.css"] ->
      let headers = Cohttp.Header.of_list [
          "Content-type", "text/css"
        ] in
      S.respond_string ~headers ~status:`OK ~body:style_css ()
    | segments ->
      let on_time = Clock.time ()  in
      start_time () >>= fun st ->
      let boot = !finish_boot_ts -. st in
      let ago  = on_time -. st in
      let body = body ~boot ~ago in
      S.respond_string ~status:`OK ~body ()

  let start c http clock =
    C.log_s c "Starting ....\n" >>= fun () ->

    (* HTTP callback *)
    let callback conn_id request body =
      let uri = S.Request.uri request in
      dispatcher (split_path uri)
    in
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in
    let mode = `TCP 80 in
    http mode (S.make ~conn_closed ~callback ())

end
