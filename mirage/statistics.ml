let statistics_page (clients, queries, blocked_requests, errors)
    (lru_weight, lru_capacity) mean_response_time (memory_live, memory_free)
    (gc_live, gc_free) =
  let elts =
    [
      ( "Total queries (" ^ string_of_int clients ^ " clients)",
        string_of_int queries );
      ("Queries resulting in an error", string_of_int errors);
      ("Queries blocked", string_of_int blocked_requests);
      ( "Percent blocked",
        if queries = 0 then "N/A"
        else
          Fmt.str "%.2f%%"
            (float_of_int blocked_requests /. float_of_int queries *. 100.) );
      ("Domains on blocklist", "82,309");
      ( "DNS cache LRU fill percentage",
        if lru_capacity = 0 then "N/A"
        else
          Fmt.str "%.2f%%"
            (float_of_int lru_weight /. float_of_int lru_capacity *. 100.) );
      ("Mean response time", Fmt.str "%u ms" mean_response_time);
      ("Live memory", string_of_int (8 * memory_live));
      ("Free memory", string_of_int (8 * memory_free));
      ("GC live memory", string_of_int (8 * gc_live));
      ("GC free memory", string_of_int (8 * gc_free));
    ]
  in
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
                  (List.mapi
                     (fun i (heading, data) ->
                       div
                         ~a:
                           [
                             (let bg =
                                match i mod 4 with
                                | 0 -> "bg-cyan-500"
                                | 1 -> "bg-cyan-600"
                                | 2 -> "bg-cyan-700"
                                | _ -> "bg-cyan-800"
                              in
                              a_class
                                [
                                  Fmt.str "%s text-white p-6 rounded shadow" bg;
                                ]);
                           ]
                         [
                           div ~a:[ a_class [ "text-sm" ] ] [ txt heading ];
                           div
                             ~a:[ a_class [ "text-2xl font-bold" ] ]
                             [ txt data ];
                         ])
                     elts);
              ];
          ];
      ])
