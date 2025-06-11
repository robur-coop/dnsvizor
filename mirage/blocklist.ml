let blocked_domains =
  [
    "googleadapis.l.google.com";
    "imasdk.googleapis.com";
    "www.example.com";
    "www.example.com.hsd1.mn.comcast.net";
    "www.example.net";
    "www.example.net.hsd1.mn.comcast.net";
    "www.example.org";
    "www.example.org.hsd1.mn.comcast.net";
  ]

let delete_button =
  Tyxml_html.(
    button
      ~a:
        [
          a_class
            [
              "bg-red-500 hover:bg-red-600 text-white font-bold py-1 px-2 \
               rounded transition duration-200";
              "focus:outline-none focus:ring-2 focus:ring-red-400";
            ];
        ]
      [ txt "Delete" ])

let blocked_row domain =
  Tyxml_html.(
    li
      ~a:[ a_class [ "flex justify-between items-center py-2 px-4" ] ]
      [ txt domain; delete_button ])

let block_page =
  Tyxml_html.(
    main
      ~a:[ a_class [ "w-full bg-gray-50 text-gray-900 min-h-screen" ] ]
      [
        section
          ~a:[ a_class [ "mx-auto px-4 sm:px-6 lg:px-8 py-12" ] ]
          [
            div
              [
                h1
                  ~a:
                    [ a_class [ "text-4xl font-extrabold mb-6 text-cyan-600" ] ]
                  [ txt "Blocklist" ];
                div
                  ~a:[ a_class [ "flex mb-4 gap-2" ] ]
                  [
                    input
                      ~a:
                        [
                          a_input_type `Text;
                          a_placeholder
                            "Add a domain (example.com or sub.example.com)";
                          a_class
                            [
                              "flex-grow border border-cyan-400 rounded px-4 \
                               py-2 w-full focus:outline-none focus:ring-2 \
                               focus:ring-cyan-300";
                            ];
                        ]
                      ();
                    button
                      ~a:
                        [
                          a_class
                            [
                              "bg-cyan-500 hover:bg-cyan-600 text-white \
                               font-semibold px-4 py-2 rounded transition \
                               duration-200";
                            ];
                        ]
                      [ txt "Add (exact)" ];
                    button
                      ~a:
                        [
                          a_class
                            [
                              "bg-cyan-500 hover:bg-cyan-600 text-white \
                               font-semibold px-4 py-2 rounded transition \
                               duration-200";
                            ];
                        ]
                      [ txt "Add (wildcard)" ];
                    button
                      ~a:
                        [
                          a_class
                            [
                              "bg-cyan-500 hover:bg-cyan-600 text-white \
                               font-semibold px-4 py-2 rounded transition \
                               duration-200";
                            ];
                        ]
                      [ txt "Add (regex)" ];
                    button
                      ~a:
                        [
                          a_class
                            [
                              "bg-cyan-100 hover:bg-cyan-200 text-cyan-700 \
                               font-semibold px-4 py-2 rounded transition \
                               duration-200 border border-cyan-300";
                            ];
                        ]
                      [ txt "⟳" ];
                  ];
                h2
                  ~a:
                    [
                      a_class
                        [ "text-2xl font-semibold mt-8 mb-4 text-cyan-700" ];
                    ]
                  [ txt "Exact blocking" ];
                ul
                  ~a:
                    [
                      a_class
                        [
                          "bg-white shadow-md rounded divide-y divide-gray-200 \
                           overflow-hidden";
                        ];
                    ]
                  (List.map blocked_row blocked_domains);
              ];
          ];
      ])
