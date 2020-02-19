module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S) (S : Mirage_stack.V4) = struct

  module Stub = Dns_stub_mirage.Make(R)(P)(M)(S)

  let start () () () () s_v4 () =
    let stub_t =
      let nameserver = Key_gen.dns_upstream ()
      and primary_t =
      (* setup DNS server state: *)
        Dns_server.Primary.create ~rng:Nocrypto.Rng.generate Dns_trie.empty
      in
      (* setup stub forwarding state and IP listeners: *)
      Stub.create ?nameserver primary_t s_v4
    in
    let _ = stub_t in

    (* Since {Stub.create} registers UDP + TCP listeners asynchronously there
       is no Lwt task.
       We need to return an infinite Lwt task to prevent the unikernel from
       exiting early: *)
    fst (Lwt.task ())
end
