open Dnsvizor.Config_parser

let msg_t =
  let pp ppf (`Msg s) = Fmt.string ppf s in
  Alcotest.testable pp (fun (`Msg a) (`Msg b) -> String.equal a b)

let dhcp_range_t = Alcotest.testable pp_dhcp_range eq_dhcp_range
let dhcp_host_t = Alcotest.testable pp_dhcp_host eq_dhcp_host
let parse_one_arg rule input = parse_one (rule arg_end_of_directive) input

let ok_dhcp_range () =
  let input = "192.168.0.50,192.168.0.150,12h" in
  let expected =
    {
      start_addr = Ipaddr.V4.of_string_exn "192.168.0.50";
      end_addr = Some (Ipaddr.V4.of_string_exn "192.168.0.150");
      mode = None;
      netmask = None;
      broadcast = None;
      lease_time = Some (12 * 60 * 60);
    }
  in
  Alcotest.(
    check
      (result dhcp_range_t msg_t)
      "DHCP range is good" (Ok expected)
      (parse_one_arg dhcp_range input))

let ok_dhcp_range_with_netmask () =
  let input = "192.168.0.50,192.168.0.150,255.255.255.0,12h" in
  let expected =
    {
      start_addr = Ipaddr.V4.of_string_exn "192.168.0.50";
      end_addr = Some (Ipaddr.V4.of_string_exn "192.168.0.150");
      mode = None;
      netmask = Some (Ipaddr.V4.of_string_exn "255.255.255.0");
      broadcast = None;
      lease_time = Some (12 * 60 * 60);
    }
  in
  Alcotest.(
    check
      (result dhcp_range_t msg_t)
      "DHCP range with netmask is good" (Ok expected)
      (parse_one_arg dhcp_range input))

let ok_dhcp_range_static () =
  (* NOTE: there's no netmask, with dnsmasq it comes from the configured
     interfaces. Unclear whether we want to support this without netmask. *)
  let input = "192.168.0.0,static" in
  let expected =
    {
      start_addr = Ipaddr.V4.of_string_exn "192.168.0.0";
      end_addr = None;
      mode = Some `Static;
      netmask = None;
      broadcast = None;
      lease_time = None;
    }
  in
  Alcotest.(
    check
      (result dhcp_range_t msg_t)
      "DHCP range with static is good" (Ok expected)
      (parse_one_arg dhcp_range input))

let make_dhcp_host ?id ?(sets = []) ?(tags = []) ?(macs = []) ?ipv4 ?ipv6
    ?lease_time ?(ignore = false) ?domain_name () =
  { id; sets; tags; macs; ipv4; ipv6; lease_time; ignore; domain_name }

let ok_dhcp_host_thedoctor () =
  let input = "00:00:5e:00:53:42,thedoctor,192.168.0.10" in
  let expected =
    make_dhcp_host
      ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:42" ]
      ~domain_name:(Domain_name.of_string_exn "thedoctor")
      ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.10")
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host thedoctor is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_tardis () =
  let input = "00:00:5e:00:53:01,00:00:5e:00:53:02,tardis,192.168.0.22" in
  let expected =
    make_dhcp_host
      ~macs:
        Macaddr.
          [
            of_string_exn "00:00:5e:00:53:01"; of_string_exn "00:00:5e:00:53:02";
          ]
      ~domain_name:(Domain_name.of_string_exn "tardis")
      ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.22")
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host tardis is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_sonicscrewdriver () =
  let input = "00:00:5e:00:53:08,sonicscrewdriver,192.168.0.23" in
  let expected =
    make_dhcp_host
      ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:08" ]
      ~domain_name:(Domain_name.of_string_exn "sonicscrewdriver")
      ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.23")
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host sonicscrewdriver is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_apollon () =
  let input = "a2:54:00:42:6a:43,apollon,10.10.10.51,infinite" in
  let expected =
    make_dhcp_host
      ~macs:[ Macaddr.of_string_exn "a2:54:00:42:6a:43" ]
      ~domain_name:(Domain_name.of_string_exn "apollon")
      ~ipv4:(Ipaddr.V4.of_string_exn "10.10.10.51")
      ~lease_time:(1 lsl 32) (* infinite *)
      ()
  in
  Alcotest.(check (result dhcp_host_t msg_t))
    "DHCP host apollon is good" (Ok expected)
    (parse_one_arg dhcp_host input)

let ok_dhcp_host_dnsmasq_conf_example =
  let to_test (name, input, expected) =
    ( name,
      `Quick,
      fun () ->
        Alcotest.(check (result dhcp_host_t msg_t))
          (name ^ " is good") (Ok expected)
          (parse_one_arg dhcp_host input) )
  in
  List.map to_test
    [
      ( "First dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,192.168.0.60",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      ( "Second dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,fred",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~domain_name:(Domain_name.of_string_exn "fred")
          () );
      ( "Third dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,fred,192.168.0.60,45m",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~domain_name:(Domain_name.of_string_exn "fred")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          ~lease_time:(45 * 60) () );
      ( "Fourth dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,12:34:56:78:90:12,192.168.0.60",
        make_dhcp_host
          ~macs:
            Macaddr.
              [
                of_string_exn "11:22:33:44:55:66";
                of_string_exn "12:34:56:78:90:12";
              ]
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      ( "Fifth dhcp-host from dnsmasq.conf.example",
        "bert,192.168.0.70,infinite",
        make_dhcp_host
          ~domain_name:(Domain_name.of_string_exn "bert")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.70")
          ~lease_time:(1 lsl 32) () );
      ( "Sixth dhcp-host from dnsmasq.conf.example",
        "id:01:02:02:04,192.168.0.60",
        make_dhcp_host ~id:(`Client_id "\x01\x02\x02\x04")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      (* skipping seventh as it's similar to above, but is infiniband in a manner we wouldn't care about *)
      ( "Eigth dhcp-host from dnsmasq.conf.example",
        "id:marjorie,192.168.0.60",
        make_dhcp_host ~id:(`Client_id "marjorie")
          ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.60")
          () );
      ( "Ninth dhcp-host from dnsmasq.conf.example",
        "judge",
        make_dhcp_host ~domain_name:(Domain_name.of_string_exn "judge") () );
      ( "Tenth dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,ignore",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~ignore:true () );
      ( "Eleventh dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,id:*",
        make_dhcp_host ~id:`Any_client_id
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          () );
      ( "Twelvth dhcp-host from dnsmasq.conf.example",
        "11:22:33:44:55:66,set:red",
        make_dhcp_host
          ~macs:[ Macaddr.of_string_exn "11:22:33:44:55:66" ]
          ~sets:[ "red" ] () );
      (* TODO:
         ("Thirteenth dhcp-host from dnsmasq.conf.example",
           "11:22:33:*:*:*,set:red",
           _);
         ("Fourteenth dhcp-host from dnsmasq.conf.example",
           "id:00:01:00:01:16:d2:83:fc:92:d4:19:e2:d8:b2, fred, [1234::5]",
           _);
      *)
    ]

let domain_t = Alcotest.testable pp_domain eq_domain

let ok_domain () =
  let input = "home.lan" in
  let expected = (Domain_name.of_string_exn "home.lan", None) in
  Alcotest.(
    check (result domain_t msg_t) "Domain is good" (Ok expected)
      (parse_one_arg domain input))

let ok_domain2 () =
  let input = "thekelleys.org.uk,192.168.0.0/24" in
  let expected =
    ( Domain_name.of_string_exn "thekelleys.org.uk",
      Some (`Ip (Ipaddr.V4.Prefix.of_string_exn "192.168.0.0/24")) )
  in
  Alcotest.(
    check (result domain_t msg_t) "Domain is good" (Ok expected)
      (parse_one_arg domain input))

let ok_domain3 () =
  let input = "thekelleys.org.uk,192.168.0.1,192.168.7.255" in
  let expected =
    ( Domain_name.of_string_exn "thekelleys.org.uk",
      Some
        (`Ip_range
           Ipaddr.V4.(of_string_exn "192.168.0.1", of_string_exn "192.168.7.255"))
    )
  in
  Alcotest.(
    check (result domain_t msg_t) "Domain is good" (Ok expected)
      (parse_one_arg domain input))

let option_t =
  let equal a b =
    List.for_all2 String.equal a.tags b.tags
    && Option.equal String.equal a.vendor b.vendor
    && String.equal
         (Dhcp_wire.dhcp_option_to_string a.option)
         (Dhcp_wire.dhcp_option_to_string b.option)
  in
  Alcotest.testable pp_dhcp_option equal

let ok_router () =
  let input = "3,192.168.4.4" in
  let expected =
    {
      tags = [];
      vendor = None;
      option = Dhcp_wire.Routers [ Ipaddr.V4.of_string_exn "192.168.4.4" ];
    }
  in
  Alcotest.(
    check (result option_t msg_t) "DHCP option is good" (Ok expected)
      (parse_one_arg dhcp_option input));
  let input = "option:router,192.168.4.4" in
  Alcotest.(
    check (result option_t msg_t) "DHCP option is good" (Ok expected)
      (parse_one_arg dhcp_option input))

let tests =
  [
    ("DHCP range", `Quick, ok_dhcp_range);
    ("DHCP range with netmask", `Quick, ok_dhcp_range_with_netmask);
    ("DHCP range static", `Quick, ok_dhcp_range_static);
    ("DHCP host thedoctor", `Quick, ok_dhcp_host_thedoctor);
    ("DHCP host tardis", `Quick, ok_dhcp_host_tardis);
    ("DHCP host sonicscrewdriver", `Quick, ok_dhcp_host_sonicscrewdriver);
    ("DHCP host apollon", `Quick, ok_dhcp_host_apollon);
  ]
  @ ok_dhcp_host_dnsmasq_conf_example
  @ [
      ("Domain from example", `Quick, ok_domain);
      ("Domain from manpage", `Quick, ok_domain2);
      ("Domain with ip range", `Quick, ok_domain3);
      ("DHCP option router", `Quick, ok_router);
    ]

let string_of_file filename =
  let config_dir = "sample-configuration-files" in
  let file = Filename.concat config_dir filename in
  try
    let fh = open_in file in
    let content = really_input_string fh (in_channel_length fh) in
    close_in_noerr fh;
    content
  with _ -> Alcotest.failf "Error reading file %S" file

let config_t = Alcotest.testable (pp_config `File) eq_config

let test_configuration config file () =
  Alcotest.(
    check (result config_t msg_t) "DHCP configuration is good" (Ok config)
      (parse_file (string_of_file file)))

let dhcp_option_conf =
  [
    `Dhcp_option
      {
        tags = [];
        vendor = None;
        option = Dhcp_wire.Log_servers [ Ipaddr.V4.localhost ];
      };
    `Dhcp_option
      {
        tags = [ "naughties" ];
        vendor = None;
        option = Dhcp_wire.Log_servers [ Ipaddr.V4.of_string_exn "8.8.8.8" ];
      };
  ]

let carpie_conf =
  [
    `Bogus_priv;
    `Dhcp_range
      {
        start_addr = Ipaddr.V4.of_string_exn "192.168.0.10";
        end_addr = None;
        mode = Some `Static;
        netmask = None;
        broadcast = None;
        lease_time = Some (48 * 60 * 60);
      };
    `Dhcp_option
      {
        tags = [];
        vendor = None;
        option = Dhcp_wire.Routers [ Ipaddr.V4.of_string_exn "192.168.0.1" ];
      };
    `Dhcp_host
      (make_dhcp_host
         ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:42" ]
         ~domain_name:(Domain_name.of_string_exn "thedoctor")
         ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.10")
         ());
    `Dhcp_host
      (make_dhcp_host
         ~macs:
           Macaddr.
             [
               of_string_exn "00:00:5e:00:53:01";
               of_string_exn "00:00:5e:00:53:02";
             ]
         ~domain_name:(Domain_name.of_string_exn "tardis")
         ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.22")
         ());
    `Dhcp_host
      (make_dhcp_host
         ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:08" ]
         ~domain_name:(Domain_name.of_string_exn "sonicscrewdriver")
         ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.23")
         ());
    `Dhcp_host
      (make_dhcp_host
         ~macs:[ Macaddr.of_string_exn "00:00:5e:00:53:10" ]
         ~domain_name:(Domain_name.of_string_exn "satellite5")
         ~ipv4:(Ipaddr.V4.of_string_exn "192.168.0.32")
         ());
    `Domain (Domain_name.of_string_exn "home.lan", None);
  ]

let netbeez_conf =
  [
    `Dhcp_range
      {
        start_addr = Ipaddr.V4.of_string_exn "172.31.0.220";
        end_addr = Some (Ipaddr.V4.of_string_exn "172.31.0.250");
        mode = None;
        netmask = Some (Ipaddr.V4.of_string_exn "255.255.255.0");
        broadcast = None;
        lease_time = Some (12 * 60 * 60);
      };
    `Dhcp_option
      {
        tags = [];
        vendor = None;
        option = Dhcp_wire.Routers [ Ipaddr.V4.of_string_exn "172.31.0.1" ];
      };
    `Dhcp_option
      {
        tags = [];
        vendor = None;
        option = Dhcp_wire.Dns_servers [ Ipaddr.V4.of_string_exn "1.1.1.1" ];
      };
  ]

let bug_124_conf =
  [
    `Dhcp_range
      {
        start_addr = Ipaddr.V4.of_string_exn "10.99.0.100";
        end_addr = Some (Ipaddr.V4.of_string_exn "10.99.0.200");
        mode = None;
        netmask = None;
        broadcast = None;
        lease_time = Some (3 * 60);
      };
    `Dhcp_option
      {
        tags = [];
        vendor = None;
        option = Dhcp_wire.Routers [ Ipaddr.V4.of_string_exn "10.99.0.1" ];
      };
    `Dhcp_option
      {
        tags = [];
        vendor = None;
        option = Dhcp_wire.Dns_servers [ Ipaddr.V4.of_string_exn "10.99.0.2" ];
      };
    `Bogus_priv;
    `Dhcp_host
      (make_dhcp_host
         ~macs:[ Macaddr.of_string_exn "00:a0:98:f9:11:89" ]
         ~domain_name:(Domain_name.of_string_exn "something")
         ~ipv4:(Ipaddr.V4.of_string_exn "10.99.0.10")
         ());
    `Domain (Domain_name.of_string_exn "testnet.lan", None);
  ]

let config_file_tests =
  [
    ("First example", `Quick, test_configuration [] "simple.conf");
    ( "White space and comments",
      `Quick,
      test_configuration [] "whitespace-and-comments.conf" );
    ( "dhcp-option",
      `Quick,
      test_configuration dhcp_option_conf "dhcp-option.conf" );
    ( "carpie.net configuration",
      `Quick,
      test_configuration carpie_conf "carpie.conf" );
    ( "netbeez configuration",
      `Quick,
      test_configuration netbeez_conf "netbeez.conf" );
    ("issue 124", `Quick, test_configuration bug_124_conf "bug-124.conf");
  ]

let tests =
  [ ("Config tests", tests); ("Configuration file tests", config_file_tests) ]

let () =
  Logs.set_reporter @@ Logs_fmt.reporter ~dst:Format.std_formatter ();
  Logs.(set_level @@ Some Debug);
  Alcotest.run "DNSvizor tests" tests
