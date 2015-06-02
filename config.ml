open Mirage

let main =
  foreign "Dispatch.Main" (console @-> http @-> clock @-> job)

let ipv4_config =
  let address = Ipaddr.V4.of_string_exn "10.0.0.10" in
  let netmask = Ipaddr.V4.of_string_exn "255.255.255.0" in
  let gateways = [Ipaddr.V4.of_string_exn "10.0.0.1"] in
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
