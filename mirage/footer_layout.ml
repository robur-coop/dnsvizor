open Tyxml

let footer =
  Html.(
    footer
      ~a:
        [
          a_class
            [
              "relative text-sm text-cyan-50 leading-6 pb-8 pt-12 lg:pt-20 \
               text-medium bg-cyan-950 bg-gradient-to-br from-cyan-900/50 \
               via-cyan-900/50 to-cyan-950/50";
            ];
        ]
      [
        div
          ~a:
            [
              a_class
                [
                  "container mx-auto px-4 sm:px-6 lg:px-8 pb-12 lg:pb-20 grid \
                   md:grid-cols-5 items-start gap-10 md:gap-16";
                ];
            ]
          [
            a
              ~a:[ a_href "/" ]
              [
                img
                  ~a:[ a_class [ "md:w-24 w-16" ] ]
                  ~src:"https://mollymawk.robur.coop/images/robur.png"
                    (* Use the Robur logo from Mollymawk*)
                  ~alt:"Robur.coop" ();
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "DNSVizor" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/about";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "About" ];
                    a
                      ~a:
                        [
                          a_href "https://robur.coop";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Robur.coop" ];
                  ];
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Resources" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Docs" ];
                    a
                      ~a:
                        [
                          a_href "https://mirageos.org";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Mirageos.org" ];
                    a
                      ~a:
                        [
                          a_href "https://ocaml.org";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "OCaml.org" ];
                  ];
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Contact" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "https://github.com/robur-coop/mollymawk";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "GitHub" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Matrix.org" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Mastodon" ];
                  ];
              ];
            dl
              [
                dt
                  ~a:
                    [
                      a_class
                        [
                          "font-bold uppercase tracking-wider text-xs \
                           text-white mb-3";
                        ];
                    ]
                  [ txt "Legal" ];
                dd
                  ~a:[ a_class [ "font-medium flex flex-col items-start" ] ]
                  [
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Security" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Privacy policy" ];
                    a
                      ~a:
                        [
                          a_href "/";
                          a_class [ "hover:text-cyan-500 transition-colors" ];
                        ]
                      [ txt "Terms of service" ];
                  ];
              ];
          ];
        p
          ~a:[ a_class [ "text-xs text-center mb-0 mt-4" ] ]
          [ txt "Copyright © 2025 DNSVizor" ];
      ])
