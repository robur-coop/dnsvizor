val encode : X509.Signing_request.t -> string
(** [encode csr] is [csr] encoded as a string suitable to put as a
    Vendor-Identifying Vendor Class data under the Mirage Private Enterprise
    Number (49836). *)

val decode :
  string -> (X509.Signing_request.t, [> `Not_csr | `Msg of string ]) result
(** [decode s] is either
    - [Ok csr] where [csr] is a certificate signing request,
    - [Error `Not_csr] if [s] does not start with the magic prefix, or
    - [Error (`Msg _)] if decoding [s] failed with an error.

    See {!encode} for the context. *)

val encode_src : Ipaddr.t * _ Domain_name.t -> string
(** [encode_src (ipaddr, domain)] is the string encoding of [(ipaddr, domain)].
    Used in DHCP Vendor-Identifying Vendor-Specific Option for the Mirage
    Private Enterprise Number (49836) suboption 1, and is used for communicating
    what DNS server to query the domain to find the TLSA records with the signed
    certificate. *)

val decode_src :
  string -> (Ipaddr.t * [ `raw ] Domain_name.t, [> `Msg of string ]) result
(** [decode_src s] is either:
    - [Ok (ipaddr, domain)], or
    - [Error _] if the encoding is somehow invalid.

    See {!encode_src} for the context. *)
