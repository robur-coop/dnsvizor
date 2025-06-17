let statistics_page (clients, queries, blocked_requests, errors) =
  Tyxml_html.(
    main
      ~a:[ a_class [ "w-full text-gray-900" ] ]
      [
        section
          ~a:
            [
              a_class [ "relative mx-auto px-4 sm:px-6 lg:px-8 py-10 xl:pb-32" ];
            ]
          [
            div
              ~a:[ a_class [ "p-6 space-y-6 overflow-auto" ] ]
              [
                div
                  ~a:[ a_class [ "flex justify-between items-center" ] ]
                  [
                    h2
                      ~a:[ a_class [ "text-2xl font-bold" ] ]
                      [ txt "Dashboard" ];
                    span
                      ~a:
                        [
                          a_class
                            [
                              "bg-red-200 text-red-700 px-2 py-1 rounded \
                               text-sm";
                            ];
                        ]
                      [ txt "Robur" ];
                  ];
                div
                  ~a:[ a_class [ "grid grid-cols-4 gap-4" ] ]
                  [
                    div
                      ~a:
                        [
                          a_class
                            [ "bg-cyan-500 text-white p-6 rounded shadow" ];
                        ]
                      [
                        div
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt ("Total queries (" ^ string_of_int clients ^ " clients)") ];
                        div
                          ~a:[ a_class [ "text-2xl font-bold" ] ]
                          [ txt (string_of_int queries) ];
                      ];
                    div
                      ~a:
                        [
                          a_class
                            [ "bg-cyan-600 text-white p-6 rounded shadow" ];
                        ]
                      [
                        div
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt "Queries Blocked" ];
                        div
                          ~a:[ a_class [ "text-2xl font-bold" ] ]
                          [ txt (string_of_int blocked_requests) ];
                      ];
                    div
                      ~a:
                        [
                          a_class
                            [ "bg-cyan-700 text-white p-6 rounded shadow" ];
                        ]
                      [
                        div
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt "Percent Blocked" ];
                        div
                          ~a:[ a_class [ "text-2xl font-bold" ] ]
                          [ txt (Fmt.str "%.2f" (float_of_int blocked_requests /. float_of_int queries)) ];
                      ];
                    div
                      ~a:
                        [
                          a_class
                            [ "bg-cyan-800 text-white p-6 rounded shadow" ];
                        ]
                      [
                        div
                          ~a:[ a_class [ "text-sm" ] ]
                          [ txt "Domains on Blocklist" ];
                        div
                          ~a:[ a_class [ "text-2xl font-bold" ] ]
                          [ txt "82,309" ];
                      ];
                  ];
                div
                  ~a:[ a_class [ "bg-white p-4 rounded shadow" ] ]
                  [
                    h3
                      ~a:[ a_class [ "text-lg font-semibold mb-2" ] ]
                      [ txt "Total queries over last 24 hours" ];
                    div
                      ~a:
                        [
                          a_class
                            [ "h-40 bg-gray-100 flex items-end justify-around" ];
                        ]
                      [
                        div ~a:[ a_class [ "w-2 bg-cyan-500 h-16" ] ] [];
                        div ~a:[ a_class [ "w-2 bg-cyan-500 h-24" ] ] [];
                        div ~a:[ a_class [ "w-2 bg-cyan-500 h-10" ] ] [];
                      ];
                  ];
                div
                  ~a:[ a_class [ "bg-white p-4 rounded shadow" ] ]
                  [
                    h3
                      ~a:[ a_class [ "text-lg font-semibold mb-2" ] ]
                      [ txt "Client activity over last 24 hours" ];
                    div
                      ~a:
                        [
                          a_class
                            [ "h-40 bg-gray-100 flex items-end justify-around" ];
                        ]
                      [
                        div ~a:[ a_class [ "w-2 bg-cyan-500 h-10" ] ] [];
                        div ~a:[ a_class [ "w-2 bg-cyan-700 h-20" ] ] [];
                        div ~a:[ a_class [ "w-2 bg-cyan-300 h-14" ] ] [];
                      ];
                  ];
              ];
          ];
      ])
