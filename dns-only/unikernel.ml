
let argument_error = 64

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S) (S : Tcpip.Stack.V4V6) = struct

  module Stub = Dns_stub_mirage.Make(R)(Time)(P)(M)(S)
  module Ca_certs = Ca_certs_nss.Make(P)

  let start () () () () s =
    let stub_t =
      let nameservers =
        match Key_gen.dns_upstream () with
        | None -> None
        | Some ip ->
          if Key_gen.no_tls () then
            Some ([ `Plaintext (ip, Key_gen.dns_port ()) ])
          else
            let time () = Some (Ptime.v (P.now_d_ps ())) in
            let of_fp data =
              let hash, fp =
                let h_of_string = function
                  | "md5" -> Some `MD5
                  | "sha" | "sha1" -> Some `SHA1
                  | "sha224" -> Some `SHA224
                  | "sha256" -> Some `SHA256
                  | "sha384" -> Some `SHA384
                  | "sha512" -> Some `SHA512
                  | _ -> None
                in
                match String.split_on_char ':' data with
                | [] -> Logs.err (fun m -> m "empty fingerprint") ; exit argument_error
                | [ fp ] -> `SHA256, fp
                | hash :: rt -> match h_of_string (String.lowercase_ascii hash) with
                  | Some h -> h, String.concat "" rt
                  | None -> Logs.err (fun m -> m "unknown hash: %s" hash); exit argument_error
              in
              hash, try Hex.to_cstruct (`Hex fp) with
                    | Invalid_argument msg ->
                      Logs.err (fun m -> m "couldn't decode hex string %S: %s" fp msg);
                      exit argument_error
            in
            let authenticator =
              match Key_gen.tls_cert_fingerprint (), Key_gen.tls_key_fingerprint () with
              | None, None ->
                (match Ca_certs.authenticator () with
                 | Ok auth -> auth
                 | Error `Msg msg ->
                   Logs.err (fun m -> m "error retrieving ca certs: %s" msg);
                   exit argument_error)
              | Some _, Some _ ->
                Logs.err (fun m -> m "both certificate and key fingerprint provided");
                exit argument_error
              | Some fp, None ->
                let hash, fingerprint = of_fp fp in
                X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint
              | None, Some fp ->
                let hash, fingerprint = of_fp fp in
                X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint
            in
            let peer_name, ip' = match Key_gen.tls_hostname () with
              | None -> None, Some ip
              | Some h -> Some (try Domain_name.(host_exn (of_string_exn h)) with Invalid_argument msg -> Logs.err (fun m -> m "invalid host name %S: %s" h msg); exit argument_error), None
            in
            let tls = Tls.Config.client ~authenticator ?peer_name ?ip:ip' () in
            Some [ `Tls (tls, ip, if Key_gen.dns_port () = 53 then 853 else Key_gen.dns_port ()) ]
      and primary_t =
      (* setup DNS server state: *)
        Dns_server.Primary.create ~rng:Mirage_crypto_rng.generate Dns_trie.empty
      in
      (* setup stub forwarding state and IP listeners: *)
      Stub.create ?nameservers primary_t s
    in
    let _ = stub_t in

    (* Since {Stub.create} registers UDP + TCP listeners asynchronously there
       is no Lwt task.
       We need to return an infinite Lwt task to prevent the unikernel from
       exiting early: *)
    fst (Lwt.task ())
end
