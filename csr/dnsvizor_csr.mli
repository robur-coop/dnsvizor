val encode : X509.Signing_request.t -> string
val decode : string -> (X509.Signing_request.t, [> `Msg of string ]) result
