open Lwt.Infix

let argument_error = 64

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S) (N : Mirage_net.S) = struct

  module Net = struct
    (* A Mirage_net.S implementation which diverts DHCP messages to a DHCP
       server. The DHCP server needs to get the entire Ethernet frame, because
       the Ethernet source address is the address to send replies to, its IPv4
       addresses (source, destination) do not matter (since the DHCP client that
       sent this request does not have an IP address yet). ARP cannot be used
       by DHCP, because the client does not have an IP address (and thus no ARP
       replies). *)

    type error = N.error

    let pp_error = N.pp_error

    type t = {
      net : N.t ;
      dhcp_config : Dhcp_server.Config.t ;
      mutable dhcp_leases : Dhcp_server.Lease.database ;
    }

    let write t = N.write t.net

    let handle_dhcp t buf =
      match Dhcp_wire.pkt_of_buf buf (Cstruct.length buf) with
      | Error e ->
        Logs.err (fun m -> m "Can't parse packet: %s" e);
        Lwt.return_unit
      | Ok pkt ->
        let now = M.elapsed_ns () |> Duration.to_sec |> Int32.of_int in
        match Dhcp_server.Input.input_pkt t.dhcp_config t.dhcp_leases pkt now with
        | Dhcp_server.Input.Silence -> Lwt.return_unit
        | Dhcp_server.Input.Update leases ->
          t.dhcp_leases <- leases;
          Logs.debug (fun m -> m "Received packet %s - updated lease database"
                         (Dhcp_wire.pkt_to_string pkt));
          Lwt.return_unit
        | Dhcp_server.Input.Warning w ->
          Logs.warn (fun m -> m "%s" w);
          Lwt.return_unit
        | Dhcp_server.Input.Error e ->
          Logs.err (fun m -> m "%s" e);
          Lwt.return_unit
        | Dhcp_server.Input.Reply (reply, leases) ->
          t.dhcp_leases <- leases;
          Logs.debug (fun m -> m "Received packet %s" (Dhcp_wire.pkt_to_string pkt));
          N.write t.net ~size:(N.mtu t.net + Ethernet.Packet.sizeof_ethernet) (Dhcp_wire.pkt_into_buf reply) >|= fun _ ->
          Logs.debug (fun m -> m "Sent reply packet %s" (Dhcp_wire.pkt_to_string reply))

    let listen t ~header_size net =
      let dhcp_or_not buf =
        let of_interest hdr =
          let dst = hdr.Ethernet.Packet.destination in
          Macaddr.compare dst (N.mac t.net) = 0 || not (Macaddr.is_unicast dst)
        in
        match Ethernet.Packet.of_cstruct buf with
        | Ok (eth_header, _) when
            of_interest eth_header &&
            Dhcp_wire.is_dhcp buf (Cstruct.length buf) -> handle_dhcp t buf
        | _ -> net buf
      in
      N.listen t.net ~header_size dhcp_or_not

    let connect net ?(dhcp_leases = Dhcp_server.Lease.make_db ()) dhcp_config =
      { net ; dhcp_config ; dhcp_leases }

    let disconnect _ =
      Logs.warn (fun m -> m "ignoring disconnect");
      Lwt.return_unit

    let mac t = N.mac t.net

    let mtu t = N.mtu t.net

    let get_stats_counters t = N.get_stats_counters t.net

    let reset_stats_counters t = N.reset_stats_counters t.net
  end

  module ETH = Ethernet.Make(Net)
  module ARP = Arp.Make(ETH)(Time)
  module IPV4 = Static_ipv4.Make(R)(M)(ETH)(ARP)
  module IPV6 = Ipv6.Make(Net)(ETH)(R)(Time)(M)
  module IPV4V6 = Tcpip_stack_direct.IPV4V6(IPV4)(IPV6)
  module ICMP = Icmpv4.Make(IPV4)
  module UDP = Udp.Make(IPV4V6)(R)
  module TCP = Tcp.Flow.Make(IPV4V6)(Time)(M)(R)

  module S = Tcpip_stack_direct.MakeV4V6(Time)(R)(Net)(ETH)(ARP)(IPV4V6)(ICMP)(UDP)(TCP)

  module Stub = Dns_stub_mirage.Make(R)(Time)(P)(M)(S)
  module Ca_certs = Ca_certs_nss.Make(P)

  let start () () () () net =
    let v4_address = Ipaddr.V4.Prefix.address (Key_gen.ipv4 ()) in
    let mac = N.mac net in
    let dhcp_config =
      let options =
        (match Key_gen.ipv4_gateway () with None -> [] | Some x -> [ Dhcp_wire.Routers [ x ]]) @
        [ Dhcp_wire.Dns_servers [ v4_address ] ]
        (* Dhcp_wire.Domain_name __ *)
      in
      let range =
        (* assumes network being /24; also doesn't check start < stop *)
        let ip = Ipaddr.V4.to_int32 v4_address in
        let start = match Key_gen.dhcp_start () with
          | Some i -> i
          | None -> Ipaddr.V4.of_int32 (Int32.(logand 0xffffff64l (logor 0x00000064l ip)))
        and stop = match Key_gen.dhcp_end () with
          | Some i -> i
          | None -> Ipaddr.V4.of_int32 (Int32.(logand 0xfffffffel (logor 0x000000fel ip)))
        in
        Some (start, stop)
      in
      Dhcp_server.Config.make
        ?hostname:None ?default_lease_time:None ?max_lease_time:None ?hosts:None
        ~addr_tuple:(v4_address, mac) ~network:(Ipaddr.V4.Prefix.prefix (Key_gen.ipv4 ())) ~range ~options ()
    in
    let net = Net.connect net dhcp_config in
    ETH.connect net >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    ARP.add_ip arp v4_address >>= fun () ->
    IPV4.connect ~no_init:(Key_gen.ipv6_only ()) ~cidr:(Key_gen.ipv4 ()) ?gateway:(Key_gen.ipv4_gateway ()) eth arp >>= fun ipv4 ->
    IPV6.connect ~no_init:(Key_gen.ipv4_only ()) ~handle_ra:(Key_gen.accept_router_advertisements ()) ?cidr:(Key_gen.ipv6 ()) ?gateway:(Key_gen.ipv6_gateway ()) net eth >>= fun ipv6 ->
    IPV4V6.connect ~ipv4_only:(Key_gen.ipv4_only ()) ~ipv6_only:(Key_gen.ipv6_only ()) ipv4 ipv6 >>= fun ip ->
    ICMP.connect ipv4 >>= fun icmp ->
    UDP.connect ip >>= fun udp ->
    TCP.connect ip >>= fun tcp ->
    S.connect net eth arp ip icmp udp tcp >>= fun stack ->
    let stub_t =
      let nameservers =
        match Key_gen.dns_upstream () with
        | None -> None
        | Some ip ->
          if Key_gen.no_tls () then
            Some ([ `Plaintext (ip, Key_gen.dns_port ()) ])
          else
            let authenticator =
              match Key_gen.authenticator () with
              | None ->
                (match Ca_certs.authenticator () with
                 | Ok auth -> auth
                 | Error `Msg msg ->
                   Logs.err (fun m -> m "error retrieving ca certs: %s" msg);
                   exit argument_error)
              | Some str ->
                match X509.Authenticator.of_string str with
                | Error `Msg msg ->
                  Logs.err (fun m -> m "%s" msg);
                  exit argument_error
                | Ok auth ->
                  let time () = Some (Ptime.v (P.now_d_ps ())) in
                  auth time
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
      Stub.create ?nameservers primary_t stack
    in
    let _ = stub_t in
    S.listen stack
end
