## DNSvizor - DNS resolver and DHCP server

This is a MirageOS unikernel which listens for DNS requests and DHCP requests in a local network. The DNS requests are resolved, and the reply is returned to the client. Each DHCP request is answered with a reply, assigning an IP to the host.

DNSvizor comes in two flavours: one (`dns-only`) only providing DNS service, and one (`dns-and-dhcp`) providing both DNS and DHCP.

## Installation from source

To install this unikernel from source, you need to have
[opam](https://opam.ocaml.org) (>= 2.1.0) and
[ocaml](https://ocaml.org) (>= 4.12.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 4.5.0). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/robur-coop/dnsvizor.git
$ cd dns-only
$ mirage configure -t <your-favourite-target>
$ make depend
$ mirage build
```

## Installing as binary

Coming soon

## Questions?

Please open an issue if you have questions, feature requests, or comments.
