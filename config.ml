(* Copyright Robur, 2020 *)

open Mirage

let upstream_resolver =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver IP" ["dns-upstream"] in
  Key.(create "dns-upstream" Arg.(required ipv4_address doc))

let dnsvizor =
  let packages =
    [
      package "logs" ;
      package "metrics" ;
      package ~min:"4.3.1" ~sublibs:["mirage"] "dns-stub";
      package "nocrypto";
    ]
  in
  foreign
    ~keys:[Key.abstract upstream_resolver]
    ~deps:[abstract nocrypto] (* initialize rng *)
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4 @-> job)

let () =
  register "dnsvizor" [
    dnsvizor $ default_random $ default_posix_clock
    $ default_monotonic_clock $ default_time
    $ generic_stackv4 default_network ]
