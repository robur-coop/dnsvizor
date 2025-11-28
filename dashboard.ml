open Tyxml

let sidebar =
  Html.(
    div
      ~a:[ a_class [ "w-full my-6" ] ]
      [
        a
          ~a:
            [
              a_href "#main-content";
              a_class
                [
                  "absolute left-0 top-0 bg-cyan-500 text-white py-2 px-4 z-50 \
                   transform -translate-y-full focus:translate-y-0 transition";
                ];
            ]
          [ txt "Skip to main content" ];
        a
          ~a:
            [
              a_href "/dashboard";
              a_class
                [
                  "hover:bg-cyan-200 hover:text-cyan-500 font-semibold \
                   hover:font-bold cursor-pointer rounded p-4 w-full flex \
                   items-center space-x-1";
                ];
            ]
          [
            i
              ~a:[ a_class [ "fa-solid fa-chart-line text-sm text-cyan-500" ] ]
              [];
            span [ txt "Dashboard" ];
          ];
        a
          ~a:
            [
              a_href "/querylog";
              a_class
                [
                  "hover:bg-cyan-200 hover:text-cyan-500 font-semibold \
                   hover:font-bold cursor-pointer rounded p-4 w-full flex \
                   items-center space-x-1";
                ];
            ]
          [
            i
              ~a:
                [
                  a_class
                    [ "fa-regular fa-rectangle-list text-sm text-cyan-500" ];
                ]
              [];
            span [ txt "Query log" ];
          ];
        a
          ~a:
            [
              a_href "/blocklist";
              a_class
                [
                  "hover:bg-cyan-200 hover:text-cyan-500 font-semibold \
                   hover:font-bold cursor-pointer rounded p-4 w-full flex \
                   items-center space-x-1";
                ];
            ]
          [
            i
              ~a:
                [ a_class [ "fa-solid fa-road-barrier text-cyan-500 text-sm" ] ]
              [];
            span [ txt "Blocklist" ];
          ];
        a
          ~a:
            [
              a_href "/configuration";
              a_class
                [
                  "hover:bg-cyan-200 hover:text-cyan-500 font-semibold \
                   hover:font-bold cursor-pointer rounded p-4 w-full flex \
                   items-center space-x-1";
                ];
            ]
          [
            i ~a:[ a_class [ "fa-solid fa-cog text-cyan-500 text-sm" ] ] [];
            span [ txt "Configuration" ];
          ];
      ])

let dashboard_layout ?(page_title = "Dashboard | DNSvizor") ~content () =
  let page =
    Html.(
      html
        ~a:[ a_lang "en" ]
        (Header_layout.header ~page_title ())
        (body
           [
             section
               ~a:
                 [
                   a_class
                     [
                       "md:grid md:grid-cols-12 block bg-gray-100 min-h-screen";
                     ];
                 ]
               [
                 section
                   ~a:
                     [
                       a_class
                         [
                           "col-span-2 px-4 py-6 w-full text-cyan-50 mx-auto \
                            bg-cyan-950 bg-gradient-to-br from-cyan-900/50 \
                            via-cyan-900/50 to-cyan-950/50 hidden md:block";
                         ];
                     ]
                   [ sidebar ];
                 section
                   ~a:[ a_class [ "md:col-span-10" ]; a_id "main-content" ]
                   [
                     div
                       ~a:
                         [
                           a_class
                             [
                               "w-full flex justify-between items-center px-4 \
                                py-6 bg-cyan-900 bg-gradient-to-br \
                                from-cyan-950/50 via-cyan-900/50 \
                                to-cyan-950/50";
                             ];
                         ]
                       [
                         div
                           ~a:
                             [
                               a_class
                                 [
                                   "text-center text-cyan-50 md:text-5xl \
                                    text-3xl";
                                 ];
                             ]
                           [ txt "DNSvizor" ];
                         div
                           ~a:[ a_class [ "block md:hidden" ] ]
                           [
                             button
                               ~a:
                                 [
                                   a_onclick
                                     "const \
                                      el=document.getElementById('mobile-text'); \
                                      el.style.display = \
                                      el.style.display==='none' ? 'block' : \
                                      'none';";
                                   a_class
                                     [
                                       "hover:text-cyan-500";
                                       "active:text-cyan-500";
                                     ];
                                   a_aria "label" [ "Toggle sidebar" ];
                                 ]
                               [
                                 i
                                   ~a:
                                     [
                                       a_class
                                         [
                                           "fa-solid fa-bars text-3xl \
                                            text-white";
                                         ];
                                     ]
                                   [];
                               ];
                             div
                               ~a:
                                 [
                                   a_id "mobile-text";
                                   a_style "display:none;";
                                   a_class
                                     [
                                       "text-white mt-3 p-3 bg-cyan-900 \
                                        rounded-md absolute left-[50%] z-50 \
                                        shadow-lg w-full";
                                     ];
                                 ]
                               [ sidebar ];
                           ];
                       ];
                     content;
                   ];
               ];
             Footer_layout.footer;
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
