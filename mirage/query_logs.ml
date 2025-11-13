let pagination_css =
  "px-2 py-1 border rounded active:bg-cyan-600 hover:bg-cyan-300 \
   cursor-pointer focus-visible:outline focus-visible:outline-4 \
   focus-visible:outline-cyan-400 focus-visible:outline-offset-2"

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
                div
                  ~a:[ a_class [ "flex justify-between items-center mb-6" ] ]
                  [
                    h2
                      ~a:[ a_class [ "text-xl font-bold text-cyan-800 mb-4" ] ]
                      [ txt "Recent Queries (showing up to 100 queries)" ];
                    a
                      ~a:
                        [
                          a_href "/all-queries";
                          a_class
                            [
                              "text-xl bg-cyan-800 text-white \
                               hover:bg-cyan-600 p-2 rounded-md font-semibold";
                            ];
                        ]
                      [ txt "Show all queries" ];
                  ];
                div
                  ~a:[ a_class [ "mb-4 flex items-center" ] ]
                  [
                    label ~a:[ a_class [ "mr-2" ] ] [ txt "Show" ];
                    select
                      ~a:
                        [
                          a_class [ "border rounded p-1" ];
                          a_name "entries_per_page";
                          a_aria "label" [ "Entries per page" ];
                        ]
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
                              ~a:[ a_class [ "p-2 text-green-800" ] ]
                              [ txt "2018-12-19 17:49:46" ];
                            td ~a:[ a_class [ "p-2" ] ] [ txt "A" ];
                            td
                              ~a:
                                [
                                  a_class [ "p-2 text-green-800" ];
                                  a_id "domain-name";
                                ]
                              [ txt "next.robur.coop" ];
                            td ~a:[ a_class [ "p-2" ] ] [ txt "192.168.1.131" ];
                            td
                              ~a:[ a_class [ "p-2 text-green-800" ] ]
                              [ txt "OK (forwarded)" ];
                            td ~a:[ a_class [ "p-2" ] ] [ txt "CNAME (25.0ms)" ];
                            td
                              ~a:[ a_class [ "p-2" ] ]
                              [
                                form
                                  ~a:
                                    [
                                      a_action "/blocklist/add";
                                      a_method `Post;
                                      a_enctype "multipart/form-data";
                                      a_class [ "flex mb-4 gap-2" ];
                                      a_onsubmit
                                        "document.getElementById('domain-input').value \
                                         = \
                                         document.getElementById('domain-name').innerText";
                                    ]
                                  [
                                    input
                                      ~a:
                                        [
                                          a_input_type `Hidden;
                                          a_name "domain";
                                          a_id "domain-input";
                                        ]
                                      ();
                                    button
                                      ~a:
                                        [
                                          a_class
                                            [
                                              "text-white border font-semibold \
                                               hover:bg-red-100 \
                                               hover:text-red-800 bg-red-800 \
                                               rounded p-2 cursor-pointer";
                                            ];
                                        ]
                                      [ txt "Block" ];
                                  ];
                              ];
                          ];
                        (* TODO: Additional rows would be appended dynamically *)
                      ];
                  ];
                div
                  ~a:[ a_class [ "mt-4 text-sm" ] ]
                  [
                    input
                      ~a:
                        [
                          a_name "apply_filtering";
                          a_input_type `Checkbox;
                          a_checked ();
                          a_aria "label" [ "Apply Filtering" ];
                        ]
                      ();
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
                          ~a:[ a_class [ pagination_css ] ]
                          [ txt "Previous" ];
                        List.init 5 (fun i ->
                            button
                              ~a:[ a_class [ pagination_css ] ]
                              [ txt (string_of_int (i + 1)) ])
                        |> Tyxml.Html.div;
                        button ~a:[ a_class [ pagination_css ] ] [ txt "Next" ];
                      ];
                  ];
              ];
          ];
      ])
