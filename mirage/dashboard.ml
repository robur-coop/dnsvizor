open Tyxml

let dashboard_layout ?(page_title = "Dashboard | DNSvizor") ~content () =
  let page =
    Html.(
      html
        (Header_layout.header ~page_title ())
        (body
           [
             section
               ~a:[ a_class [ "grid grid-cols-12 bg-gray-100 min-h-screen" ] ]
               [
                 section
                   ~a:
                     [
                       a_class
                         [
                           "col-span-2 px-4 py-6 w-full text-cyan-50 mx-auto \
                            bg-cyan-950 bg-gradient-to-br from-cyan-900/50 \
                            via-cyan-900/50 to-cyan-950/50";
                         ];
                     ]
                   [
                     div
                       ~a:
                         [
                           a_id "alert-container";
                           a_class
                             [
                               "fixed top-1/4 rounded-md right-4 z-50 w-fit \
                                space-y-2 p-4 shadow text-wrap hidden";
                             ];
                         ]
                       [];
                     div
                       ~a:[ a_class [ "w-full my-6" ] ]
                       [
                         a
                           ~a:
                             [
                               a_href "/dashboard";
                               a_class
                                 [
                                   "hover:bg-cyan-200 hover:text-cyan-500 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-4 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-chart-line text-sm \
                                        text-cyan-500";
                                     ];
                                 ]
                               [];
                             span [ txt "Dashboard" ];
                           ];
                         a
                           ~a:
                             [
                               a_href "/querylog";
                               a_class
                                 [
                                   "hover:bg-cyan-200 hover:text-cyan-500 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-4 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-regular fa-rectangle-list text-sm \
                                        text-cyan-500";
                                     ];
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
                                   "hover:bg-cyan-200 hover:text-cyan-500 \
                                    font-semibold hover:font-bold \
                                    cursor-pointer rounded p-4 w-full flex \
                                    items-center space-x-1";
                                 ];
                             ]
                           [
                             i
                               ~a:
                                 [
                                   a_class
                                     [
                                       "fa-solid fa-road-barrier text-cyan-500 \
                                        text-sm";
                                     ];
                                 ]
                               [];
                             span [ txt "Block list" ];
                           ];
                       ];
                   ];
                 section
                   ~a:[ a_class [ "col-span-10" ] ]
                   [
                     div
                       ~a:
                         [
                           a_class
                             [
                               "px-4 py-6 bg-cyan-900 bg-gradient-to-br \
                                from-cyan-950/50 via-cyan-900/50 \
                                to-cyan-950/50 text-center text-cyan-50 \
                                text-5xl";
                             ];
                         ]
                       [ txt "DNSvizor" ];
                     content;
                   ];
               ];
             Footer_layout.footer;
           ]))
  in
  Format.asprintf "%a" (Html.pp ()) page
