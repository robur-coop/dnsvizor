let blocked_nameserver = Domain_name.of_string_exn "blocked"

let hostmaster_of_blocklist_source source =
  (* TODO: sanitize [blocklist_source]:
     - Two consecutive dots are disallowed,
     - A trailing dot is "removed". *)
  Domain_name.of_string_exn source

let soa blocklist_source =
  let hostmaster = hostmaster_of_blocklist_source blocklist_source in
  Dns.Soa.create blocked_nameserver
    ~hostmaster

let blocklist_source_of_soa soa : string option =
  let s = Domain_name.to_string soa.Dns.Soa.hostmaster in
  if String.starts_with s ~prefix:"https://" ||
     String.starts_with s ~prefix:"http://" then
    Some s
  else None

let add_dns_entries blocklist_source trie name =
  Dns_trie.insert name Dns.Rr_map.Soa (soa blocklist_source) trie

let update_from_source trie source blocklist =
  Logs.info (fun m -> m "Applying blocklist from %S: %a" source Fmt.(Dump.list Domain_name.pp) blocklist);
  let to_remove =
    Dns_trie.fold Dns.Rr_map.Soa trie
      (fun nam soa acc ->
         if Domain_name.equal soa.nameserver blocked_nameserver &&
            Domain_name.equal soa.hostmaster
              (hostmaster_of_blocklist_source source) &&
            not (List.exists (Domain_name.equal nam) blocklist)
         then
           nam :: acc
         else acc)
      []
  in
  let trie =
    List.fold_left
      (fun acc nam -> Dns_trie.remove_all nam acc)
      trie
      to_remove
  in
  List.fold_left (add_dns_entries source) trie blocklist

let blocked_domains trie =
  Dns_trie.fold Dns.Rr_map.Soa trie
    (fun nam soa acc ->
       if Domain_name.equal soa.nameserver blocked_nameserver then
         (nam, soa) :: acc
       else acc)
    []

let delete_button domain =
  Tyxml_html.(
    form
      ~a:[
        Fmt.kstr a_action "/blocklist/delete/%a" Domain_name.pp domain;
        a_method `Post;
      ]
      [
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
          [ txt "Delete" ]
      ])

let blocked_row (domain, soa) =
  Tyxml_html.(
    li
      ~a:[ a_class [ "flex justify-between items-center py-2 px-4" ] ]
      [ txt (Domain_name.to_string ~trailing:true domain);
        div [
          match blocklist_source_of_soa soa with
          | None -> txt "Unknown"
          | Some source -> a ~a:[a_href source] [ txt source ]
        ];
        delete_button domain ])

let block_page blocked_domains =
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
                form
                  ~a:[
                    a_action "/blocklist/add";
                    a_method `Post;
                    a_class [ "flex mb-4 gap-2" ]
                  ]
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
                      [ txt "Add" ];
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
                  [ txt "Blocked domains" ];
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
