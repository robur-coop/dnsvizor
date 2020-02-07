open Lwt.Infix

module Main (R : Mirage_random.S) (P : Mirage_clock.PCLOCK)
    (M : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S) (N : Mirage_net.S) = struct
  module ETH = Ethernet.Make(N)
  module ARP = Arp.Make(ETH)(Time)
  module IPV4 = Static_ipv4.Make(R)(M)(ETH)(ARP)
  module ICMP = Icmpv4.Make(IPV4)
  module UDP = Udp.Make(IPV4)(R)
  module TCP = Tcp.Flow.Make(IPV4)(Time)(M)(R)

  module S = struct
    (* A Mirage_stack.V4 implementation which diverts DHCP messages to a DHCP
       server. The DHCP server needs to get the entire Ethernet frame, because
       the Ethernet source address is the address to send replies to, its IPv4
       addresses (source, destination) do not matter (since the DHCP client that
       sent this request does not have an IP address yet). ARP cannot be used
       by DHCP, because the client does not have an IP address (and thus no ARP
       replies). *)

    module UDPV4 = UDP
    module TCPV4 = TCP
    module IPV4 = IPV4

    type t = {
      net : N.t ; eth : ETH.t ; arp : ARP.t ;
      ip : IPV4.t ; icmp : ICMP.t ; udp : UDP.t ; tcp : TCP.t ;
      udp_listeners : (int, UDP.callback) Hashtbl.t ;
      tcp_listeners : (int, TCP.listener) Hashtbl.t ;
      dhcp_config : Dhcp_server.Config.t ;
      dhcp_leases : Dhcp_server.Lease.database ;
    }

    let ipv4 { ip ; _ } = ip
    let udpv4 { udp ; _ } = udp
    let tcpv4 { tcp ; _ } = tcp

    let listener h port = Hashtbl.find_opt h port
    let udp_listener h ~dst_port = listener h dst_port

    let listen_udpv4 { udp_listeners ; _ } ~port cb =
      Hashtbl.replace udp_listeners port cb

    let listen_tcpv4 ?keepalive { tcp_listeners ; _ } ~port cb =
      Hashtbl.replace tcp_listeners port { TCP.process = cb ; TCP.keepalive }

    let handle_dhcp _t _buf =
      Logs.warn (fun m -> m "received DHCP packet");
      Lwt.return_unit

    let listen t =
      let ethif_listener =
        ETH.input
          ~arpv4:(ARP.input t.arp)
          ~ipv4:(
            IPV4.input
              ~tcp:(TCP.input t.tcp ~listeners:(listener t.tcp_listeners))
              ~udp:(UDP.input t.udp ~listeners:(udp_listener t.udp_listeners))
              ~default:(fun ~proto ~src ~dst buf ->
                  match proto with
                  | 1 -> ICMP.input t.icmp ~src ~dst buf
                  | _ -> Lwt.return_unit)
              t.ip)
          ~ipv6:(fun _ -> Lwt.return_unit)
          t.eth
      in
      let dhcp_or_not buf =
        let of_interest hdr =
          let dst = hdr.Ethernet_packet.destination in
          Macaddr.compare dst (N.mac t.net) = 0 || not (Macaddr.is_unicast dst)
        in
        match Ethernet_packet.Unmarshal.of_cstruct buf with
        | Ok (eth_header, _) when
            of_interest eth_header &&
            Dhcp_wire.is_dhcp buf (Cstruct.len buf) -> handle_dhcp t buf
        | _ -> ethif_listener buf
      in
      N.listen t.net ~header_size:Ethernet_wire.sizeof_ethernet dhcp_or_not
      >>= function
      | Error e ->
        Logs.warn (fun p -> p "%a" N.pp_error e) ;
        Lwt.return_unit
      | Ok _res -> Lwt.return_unit

    let connect net eth arp ip icmp udp tcp ?(dhcp_leases = Dhcp_server.Lease.make_db ()) dhcp_config =
      { net ; eth ; arp ; ip ; icmp ; udp ; tcp ;
        udp_listeners = Hashtbl.create 2 ;
        tcp_listeners = Hashtbl.create 2 ;
        dhcp_config ; dhcp_leases
      }

    let disconnect _ =
      Logs.warn (fun m -> m "ignoring disconnect");
      Lwt.return_unit
  end

  module Stub = Dns_stub_mirage.Make(R)(P)(M)(S)

  let start () () () () net _nocrypto =
    let network, ip_address = Key_gen.ipv4 ()
    and gateway = Key_gen.ipv4_gateway ()
    in
    ETH.connect net >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    ARP.add_ip arp ip_address >>= fun () ->
    IPV4.connect ~ip:(network, ip_address) ~gateway eth arp >>= fun ip ->
    ICMP.connect ip >>= fun icmp ->
    UDP.connect ip >>= fun udp ->
    TCP.connect ip >>= fun tcp ->
    let mac = N.mac net in
    let dhcp_config =
      let addr_tuple = (ip_address, mac) in
      Dhcp_server.Config.make
        ?hostname:None ?default_lease_time:None ?max_lease_time:None ?hosts:None
        ~addr_tuple ~network ~range:None ~options:[]
    in
    let stack = S.connect net eth arp ip icmp udp tcp dhcp_config in
    let stub_t =
      let nameserver = Key_gen.dns_upstream ()
      and primary_t =
      (* setup DNS server state: *)
        Dns_server.Primary.create ~rng:Nocrypto.Rng.generate Dns_trie.empty
      in
      (* setup stub forwarding state and IP listeners: *)
      Stub.create ?nameserver primary_t stack
    in
    let _ = stub_t in
    S.listen stack
end
