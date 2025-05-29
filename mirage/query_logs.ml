let query_page =
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
            section
              ~a:[ a_class [ "px-4 py-6 w-full" ] ]
              [
                h2
                  ~a:[ a_class [ "text-xl font-bold text-cyan-800 mb-4" ] ]
                  [
                    txt "Recent Queries (showing up to 100 queries), ";
                    a ~a:[ a_href "/all-queries" ] [ txt "show all" ];
                  ];
                div
                  ~a:[ a_class [ "mb-4 flex items-center" ] ]
                  [
                    label ~a:[ a_class [ "mr-2" ] ] [ txt "Show" ];
                    select
                      ~a:[ a_class [ "border rounded p-1" ] ]
                      (List.map
                         (fun n ->
                           option
                             ~a:[ a_value (string_of_int n) ]
                             (txt (string_of_int n)))
                         [ 10; 25; 50; 100 ]);
                    span ~a:[ a_class [ "ml-2" ] ] [ txt "entries" ];
                  ];
                div
                  ~a:[ a_class [ "overflow-x-auto" ] ]
                  [
                    table
                      ~a:
                        [
                          a_class
                            [ "w-full table-auto border-collapse text-sm" ];
                        ]
                      ~thead:
                        (thead
                           [
                             tr
                               (List.map
                                  (fun heading ->
                                    th
                                      ~a:
                                        [
                                          a_class
                                            [
                                              "border-b p-2 text-left \
                                               font-semibold text-gray-700";
                                            ];
                                        ]
                                      [ txt heading ])
                                  [
                                    "Time";
                                    "Type";
                                    "Domain";
                                    "Client";
                                    "Status";
                                    "Reply";
                                    "Action";
                                  ]);
                           ])
                      [
                        tr
                          [
                            td
                              ~a:[ a_class [ "p-2 text-green-600" ] ]
                              [ txt "2018-12-19 17:49:46" ];
                            td ~a:[ a_class [ "p-2" ] ] [ txt "A" ];
                            td
                              ~a:[ a_class [ "p-2 text-green-700" ] ]
                              [ txt "next.robur.coop" ];
                            td ~a:[ a_class [ "p-2" ] ] [ txt "192.168.1.131" ];
                            td
                              ~a:[ a_class [ "p-2 text-green-600" ] ]
                              [ txt "OK (forwarded)" ];
                            td ~a:[ a_class [ "p-2" ] ] [ txt "CNAME (25.0ms)" ];
                            td
                              ~a:[ a_class [ "p-2" ] ]
                              [
                                button
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "text-red-600 border border-red-600 \
                                           rounded px-2";
                                        ];
                                    ]
                                  [ txt "Block" ];
                              ];
                          ];
                        (* TODO: Additional rows would be appended dynamically *)
                      ];
                  ];
                div
                  ~a:[ a_class [ "mt-4 text-sm" ] ]
                  [
                    input ~a:[ a_input_type `Checkbox; a_checked () ] ();
                    label
                      ~a:[ a_class [ "ml-2" ] ]
                      [
                        txt
                          "Apply filtering on click on Type, Domain, and \
                           Clients";
                      ];
                  ];
                div
                  ~a:[ a_class [ "mt-6 flex justify-between items-center" ] ]
                  [
                    span [ txt "Showing 1 to 10 of 100 entries" ];
                    div
                      ~a:
                        [
                          a_class
                            [ "space-x-1 flex justify-between items-center" ];
                        ]
                      [
                        button
                          ~a:
                            [
                              a_class
                                [ "px-2 py-1 border rounded text-gray-500" ];
                            ]
                          [ txt "Previous" ];
                        List.init 5 (fun i ->
                            button
                              ~a:[ a_class [ "px-2 py-1 border rounded" ] ]
                              [ txt (string_of_int (i + 1)) ])
                        |> Tyxml.Html.div;
                        button
                          ~a:
                            [
                              a_class
                                [ "px-2 py-1 border rounded text-gray-500" ];
                            ]
                          [ txt "Next" ];
                      ];
                  ];
              ];
          ];
      ])
