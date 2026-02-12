let prefix = "csr\000"
let prefix_len = String.length prefix

let encode csr =
  let der = X509.Signing_request.encode_der csr in
  let b = Bytes.create (String.length prefix + 2 + String.length der) in
  Bytes.blit_string prefix 0 b 0 prefix_len;
  Bytes.set_uint16_be b prefix_len (String.length der);
  Bytes.blit_string der 0 b (prefix_len + 2) (String.length der);
  Bytes.unsafe_to_string b

let decode s =
  let ( let* ) = Result.bind in
  let* len =
    if String.length s < prefix_len then Error (`Msg "Truncated")
    else if String.starts_with ~prefix s then
      Ok (String.get_uint16_be s prefix_len)
    else Error (`Msg "Not a CSR")
  in
  let off = prefix_len + 2 in
  let* data =
    (* TODO(reynir): we don't do anything with slack at the end; revise *)
    if String.length s < off + len then Error (`Msg "Truncated")
    else Ok (String.sub s off len)
  in
  X509.Signing_request.decode_der data



let encode_src (ip, domain) =
  (match ip with
   | Ipaddr.V4 ip ->
     "4" ^ Ipaddr.V4.to_octets ip
   | Ipaddr.V6 ip ->
     "6" ^ Ipaddr.V6.to_octets ip) ^
  Domain_name.to_string domain

let decode_src s =
  let ( let* ) = Result.bind in
  let* v =
    if String.length s < 1 then
      Error (`Msg "Truncated")
    else match String.get s 0 with
      | '4' -> Ok `V4
      | '6' -> Ok `V6
      | _illegal -> Error (`Msg "Bad ip type")
  in
  let* ip, off =
    match v with
    | `V4 ->
      if String.length s < 1 + 4 then
        Error (`Msg "Truncated")
      else
        let* ipv4 = Ipaddr.V4.of_octets ~off:1 s in
        Ok (Ipaddr.V4 ipv4, 1 + 4)
    | `V6 ->
      if String.length s < 1 + 16 then
        Error (`Msg "Truncated")
      else
        let* ipv6 = Ipaddr.V6.of_octets ~off:1 s in
        Ok (Ipaddr.V6 ipv6, 1 + 16)
  in
  let* domain = Domain_name.of_string (String.sub s off (String.length s - off)) in
  Ok (ip, domain)
