open Lwt
open Printf
open V1_LWT

let pb = Printf.bprintf

module Index = struct

  let header =
    "<meta charset=\"utf-8\">\n\
     <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
     <title>Index</title>\n\
     <link rel=\"stylesheet\" href=\"css/foundation.css\">\n\
     <script src=\"js/vendor/modernizr.js\"></script>"

  let footer =
    "<script src=\"js/vendor/jquery.js\"></script>\n\
     <script src=\"js/foundation.min.js\"></script>\n\
     <script>\n\
    \  $(document).foundation();\n\
     </script>"

  let time ~boot ~ago =
    Printf.sprintf
      "<div class=\"row\">\n\
      \  <div class=\"large-12 columns\">\n\
      \     <div class=\"panel\">\n\
      \       Unikernel booted in %f seconds, %f seconds ago\n\
      \    </div>\n\
      \  </div>\n\
       </div>" boot ago

  let manifest () =
    let open Opam_manifest in
    let buf = Buffer.create 1024 in
    pb buf "<div class=\"large-4 columns\">\n\
            <h3>Build Manifest<br/><small>%d packages</small></h3>\n\
            <table>\n\
            <thead><tr><th>Name</th><th>Version</th></tr></thead>"
      (List.length Opam_manifest.all);
    List.iter (fun pkg ->
        match pkg.archive with
        | "" -> pb buf "<tr><td>%s</td><td>%s</td></tr>\n" pkg.name pkg.version
        | a  -> pb buf "<tr><td>%s</td><td><a href=%S>%s</a></td></tr>\n"
                  pkg.name a pkg.version
      ) Opam_manifest.all;
    pb buf "</table>\n\
            </div>";
    Buffer.contents buf

  let gc () =
    let open Gc in
    let k f = Printf.sprintf "%dk" (f / 1_000) in
    let m f = Printf.sprintf "%.0fm" (f /. 1_000_000.) in
    let t = Gc.stat () in
    Printf.sprintf
      "<div class=\"large-4 columns\">\n\
       <h3>Live GC Stats</h3>
       <table>\n\
       <tr><td>Allocated Bytes</td><td>%s</td></tr>\n\
       <tr><td>Head Words</td><td>%s</td></tr>\n\
       <tr><td>Love Words</td><td>%s</td></tr>\n\
       </table>\n\
       </div>"
      (m (Gc.allocated_bytes ()))
      (k t.heap_words)
      (k t.live_words)

  let create ~boot ~ago =
    Printf.sprintf
      "<!doctype html>\n\
       <html class=\"no-js\ lang=\"en\">\n\
       <head>\n\
       %s\n\
       </head>\n\
       <body>\n\
       <div class=\"row\">
       <h1>Hello World from Jitsu!</h1>\
       </div>
       <div class=\"row\">\n\
       %s\n\
       %s\n\
       %s\n\
       </div>\n\
       %s\n\
       </body>\n\
       </html>"
      header (time ~boot ~ago) (gc ()) (manifest ()) footer

end

(* Split a URI into a list of path segments *)
let split_path uri =
  let path = Uri.path uri in
  Re_str.(split_delim (regexp_string "/") path)
  |> List.filter (fun e -> e <> "")

module Main (C:CONSOLE) (KV: KV_RO) (S:Cohttp_lwt.Server) (Clock : V1.CLOCK) =
struct

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

  let read_static kv name =
    KV.size kv name >>= function
    | `Error (KV.Unknown_key _) -> Lwt.return_none
    | `Ok size ->
      KV.read kv name 0 (Int64.to_int size) >>= function
      | `Error (KV.Unknown_key _) -> Lwt.return_none
      | `Ok bufs -> Lwt.return (Some ((Cstruct.copyv bufs)))

  (* dispatch non-file URLs *)
  let rec dispatcher kv = function
    | [] -> dispatcher kv ["index.html"]
    | ["index.html"] ->
      let on_time = Clock.time ()  in
      start_time () >>= fun st ->
      let boot = !finish_boot_ts -. st in
      let ago  = on_time -. st in
      let body = Index.create ~boot ~ago in
      S.respond_string ~status:`OK ~body ()
    | path ->
      let path = String.concat "/" path in
      let mimetype = Magic_mime.lookup path in
      let headers = Cohttp.Header.of_list [
          "contents-type", mimetype
        ] in
      read_static kv path >>= function
      | None   -> S.respond_not_found ()
      | Some s -> S.respond_string ~headers ~status:`OK ~body:s ()

  let start c kv http clock =
    C.log_s c "Starting ....\n" >>= fun () ->

    (* HTTP callback *)
    let callback conn_id request body =
      let uri = Cohttp.Request.uri request in
      dispatcher kv (split_path uri)
    in
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in
    let mode = `TCP 80 in
    http mode (S.make ~conn_closed ~callback ())

end
