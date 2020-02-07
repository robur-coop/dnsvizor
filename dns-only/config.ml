(* Copyright Robur, 2020 *)

open Mirage

let upstream_resolver =
  let doc = Key.Arg.info ~doc:"Upstream DNS resolver IP" ["dns-upstream"] in
  Key.(create "dns-upstream" Arg.(opt (some ipv4_address) None doc))

let dnsvizor =
  let pin = "git+https://github.com/mirage/ocaml-dns.git" in
  let packages =
    [
      package "logs" ;
      package "metrics" ;
      package ~pin ~sublibs:["mirage"] "dns-stub";
      package ~pin "dns";
      package ~pin "dns-client";
      package ~pin "dns-mirage";
      package ~pin "dns-resolver";
      package ~pin "dns-tsig";
      package ~pin "dns-server";
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
