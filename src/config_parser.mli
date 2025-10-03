type dhcp_range = {
  start_addr : Ipaddr.V4.t;
  end_addr : Ipaddr.V4.t option;
  mode : [ `Static | `Proxy ] option;
  netmask : Ipaddr.V4.t option;
  broadcast : Ipaddr.V4.t option;
  lease_time : int option;
}

val dhcp_range : unit Angstrom.t -> dhcp_range Angstrom.t
val pp_dhcp_range : dhcp_range Fmt.t
val dhcp_range_docv : string
val dhcp_range_c : dhcp_range Cmdliner.Arg.conv

type dhcp_host = {
  id : [ `Any_client_id | `Client_id of string ] option;
  sets : string list;
  tags : string list;
  macs : Macaddr.t list;
  ipv4 : Ipaddr.V4.t option;
  ipv6 : Ipaddr.V6.t option;
  lease_time : int option;
  ignore : bool;
  (* TODO: [`host] Domain_name.t?! *)
  domain_name : [ `raw ] Domain_name.t option;
}

val dhcp_host : unit Angstrom.t -> dhcp_host Angstrom.t
val pp_dhcp_host : dhcp_host Fmt.t
val dhcp_host_docv : string
val dhcp_host_c : dhcp_host Cmdliner.Arg.conv

type dhcp_option = { tags : string list; option : Dhcp_wire.dhcp_option }

val dhcp_option : unit Angstrom.t -> dhcp_option Angstrom.t
val pp_dhcp_option : dhcp_option Fmt.t
val dhcp_option_docv : string
val dhcp_option_c : dhcp_option Cmdliner.Arg.conv

type domain =
  [ `raw ] Domain_name.t
  * [ `Interface of string
    | `Ip of Ipaddr.V4.Prefix.t
    | `Ip_range of Ipaddr.V4.t * Ipaddr.V4.t ]
    option

val domain : unit Angstrom.t -> domain Angstrom.t
val pp_domain : domain Fmt.t
val domain_docv : string
val domain_c : domain Cmdliner.Arg.conv
val ignore_c : string -> string Cmdliner.Arg.conv

type config =
  [ `Dhcp_range of dhcp_range
  | `Domain of domain
  | `Dhcp_option of dhcp_option
  | `Dnssec
  | `Ignored ]
  list

val parse_file : string -> (config, [> `Msg of string ]) result
val arg_end_of_directive : unit Angstrom.t
val parse_one : 'a Angstrom.t -> string -> ('a, [> `Msg of string ]) result
