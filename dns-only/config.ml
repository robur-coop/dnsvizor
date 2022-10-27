(* Copyright Robur, 2020 *)

open Mirage

let dns_upstream =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver" ["dns-upstream"] in
  Key.(create "dns-upstream" Arg.(opt (some string) None doc))

let dns_cache =
  let doc = Key.Arg.info ~doc:"DNS cache size" ["dns-cache"] in
  Key.(create "dns-cache" Arg.(opt (some int) None doc))

let dnsvizor =
  let packages =
    [
      package "logs" ;
      package "metrics" ;
      package ~min:"6.4.0" ~sublibs:["mirage"] "dns-stub";
      package "dns";
      package "dns-client";
      package "dns-mirage";
      package "dns-resolver";
      package "dns-tsig";
      package "dns-server";
      package ~min:"4.3.1" "mirage-runtime";
    ]
  in
  foreign
    ~keys:[Key.v dns_upstream; Key.v dns_cache]
    ~packages
    "Unikernel.Main"
    (random @-> pclock @-> mclock @-> time @-> stackv4v6 @-> job)

let () =
  register "dns-stub" [
    dnsvizor $ default_random $ default_posix_clock
    $ default_monotonic_clock $ default_time
    $ generic_stackv4v6 default_network ]
