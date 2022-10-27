module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S) (S : Tcpip.Stack.V4V6) = struct

  module Stub = Dns_stub_mirage.Make(R)(Time)(P)(M)(S)

  let start () () () () s =
    let stub_t =
      let nameservers =
        Option.map (fun ns -> [ ns ]) (Key_gen.dns_upstream ())
      and primary_t =
      (* setup DNS server state: *)
        Dns_server.Primary.create ~rng:Mirage_crypto_rng.generate Dns_trie.empty
      in
      (* setup stub forwarding state and IP listeners: *)
      try
        Stub.create ?cache_size:(Key_gen.dns_cache ()) ?nameservers primary_t s
      with
        Invalid_argument a ->
        Logs.err (fun m -> m "error %s" a);
        exit Mirage_runtime.argument_error
    in
    let _ = stub_t in

    (* Since {Stub.create} registers UDP + TCP listeners asynchronously there
       is no Lwt task.
       We need to return an infinite Lwt task to prevent the unikernel from
       exiting early: *)
    fst (Lwt.task ())
end
