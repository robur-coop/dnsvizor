val encode : X509.Signing_request.t -> string

val decode :
  string -> (X509.Signing_request.t, [> `Not_csr | `Msg of string ]) result

val encode_src : Ipaddr.t * _ Domain_name.t -> string

val decode_src :
  string -> (Ipaddr.t * [ `raw ] Domain_name.t, [> `Msg of string ]) result
