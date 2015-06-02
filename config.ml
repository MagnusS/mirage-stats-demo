open Mirage

let get_exn name fn =
  let res = Sys.getenv name in
  Printf.printf "\027[33mENV\027[m         %s => %s\n%!" name res;
  fn (String.lowercase res)
let get name ~default fn = try get_exn name fn with Not_found -> default
let ip name default =
  let fn = Ipaddr.V4.of_string_exn in
  get name ~default:(fn default) fn

let main =
  foreign "Dispatch.Main" (console @-> http @-> clock @-> job)

let ipv4_config =
  let address  = ip "IP" "10.0.0.10" in
  let netmask  = ip "NM" "255.255.255.0" in
  let gateways = [ip "GW" "10.0.0.1"] in
  { address; netmask; gateways }

let stack console = direct_stackv4_with_static_ipv4 console tap0 ipv4_config

let server =
  conduit_direct (stack default_console)

let http_srv =
  http_server server

let () =
  add_to_ocamlfind_libraries ["re.str";"str"];

  register "www" [
    main $ default_console $ http_srv $ default_clock
  ]
