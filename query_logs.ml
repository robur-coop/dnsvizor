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
                  ];
              ];
            div
              ~a:[ a_class [ "overflow-x-auto" ] ]
              [
                table
                  ~a:
                    [
                      a_id "query-logs";
                      a_class [ "w-full table-auto border-collapse text-sm" ];
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
                                "Return code";
                                "Time taken";
                                "Status";
                                "Action";
                              ]);
                       ])
                  [];
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
              [ txt "Apply filtering on click on Type, Domain, and Clients" ];
          ];
      ])
