let blocked_nameserver = Domain_name.of_string_exn "blocked"

(* Tested with:
   {[
     let test =
       QCheck.Test.make ~count:1000 ~name:"hostmaster_of_blocklist_source no exception"
         QCheck.string
         (fun s -> let _ = hostmaster_of_blocklist_source s in true)
     in
     QCheck.Test.check_exn test;;
   ]} *)
let hostmaster_of_blocklist_source source =
  (* sanitize [blocklist_source]:
     - Two consecutive dots are disallowed,
     - labels are at most 63 bytes
     - the whole string is at most 255 bytes*)
  let labels = String.split_on_char '.' source in
  let rev_labels, _ =
    List.fold_left
      (fun (acc, run_length) label ->
        let len = min 63 (min (String.length label) (255 - succ run_length)) in
        if len <= 0 then (acc, run_length)
        else if String.length label > len then
          (String.sub label 0 len :: acc, run_length + len + 1)
        else (label :: acc, run_length + len + 1))
      ([], 1) labels
  in
  let d = Domain_name.of_strings_exn (List.rev rev_labels) in
  Logs.info (fun m ->
      let d_str = Domain_name.to_string d in
      if not (String.equal source d_str) then
        m "Blocklist source %S is domain'ified to %S" source d_str);
  d

let soa blocklist_source serial =
  let hostmaster = hostmaster_of_blocklist_source blocklist_source in
  Dns.Soa.create blocked_nameserver ~hostmaster ~serial

let blocklist_source_of_soa soa : string option =
  let s = Domain_name.to_string soa.Dns.Soa.hostmaster in
  if
    String.starts_with s ~prefix:"https://"
    || String.starts_with s ~prefix:"http://"
  then Some s
  else None

let add_single_block source trie name =
  Dns_trie.insert name Dns.Rr_map.Soa (soa source 0l) trie

let remove_old_serial trie source serial =
  Dns_trie.fold Dns.Rr_map.Soa trie
    (fun nam soa acc ->
      if
        Domain_name.equal soa.nameserver blocked_nameserver
        && Domain_name.equal soa.hostmaster
             (hostmaster_of_blocklist_source source)
        && soa.serial <> serial
      then Dns_trie.remove_all nam acc
      else acc)
    trie

module SM = Map.Make (String)

let blocked_domains trie =
  let incr lists s =
    match SM.find_opt s lists with
    | None -> SM.add s 0 lists
    | Some n -> SM.add s (succ n) lists
  in
  Dns_trie.fold Dns.Rr_map.Soa trie
    (fun nam soa (manual, lists) ->
      if Domain_name.equal soa.nameserver blocked_nameserver then
        match blocklist_source_of_soa soa with
        | Some s -> (manual, incr lists s)
        | None -> (nam :: manual, lists)
      else (manual, lists))
    ([], SM.empty)

let number_of_blocked_domains trie =
  Dns_trie.fold Dns.Rr_map.Soa trie
    (fun _ soa acc ->
      if Domain_name.equal soa.nameserver blocked_nameserver then succ acc
      else acc)
    0

let delete_button domain =
  Tyxml_html.(
    button
      ~a:
        [
          a_form "delete-form";
          a_formmethod `Post;
          Fmt.kstr a_formaction "/blocklist/delete/%a" Domain_name.pp domain;
          a_class
            [
              "bg-red-500 hover:bg-red-600 text-white font-bold py-1 px-2 \
               rounded transition duration-200 cursor-pointer";
              "focus:outline-none focus:ring-2 focus:ring-red-400";
            ];
        ]
      [ txt "Delete" ])

let blocked_row domain =
  Tyxml_html.(
    tr
      ~a:[ a_class [ "items-center py-2 px-4" ] ]
      [
        td [ txt (Domain_name.to_string ~trailing:true domain) ];
        td [ delete_button domain ];
      ])

let block_page (manual_blocked_domains, lists) =
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
                  ~a:
                    [
                      a_action "/blocklist/add";
                      a_method `Post;
                      a_class [ "flex mb-4 gap-2" ];
                    ]
                  [
                    input
                      ~a:
                        [
                          a_input_type `Text;
                          a_name "domain";
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
                               duration-200 cursor-pointer";
                            ];
                        ]
                      [ txt "Add" ];
                    button
                      ~a:
                        [
                          a_formaction "/blocklist/update";
                          a_formmethod `Post;
                          a_class
                            [
                              "bg-cyan-100 hover:bg-cyan-200 text-cyan-700 \
                               font-semibold px-4 py-2 rounded transition \
                               duration-200 border border-cyan-300 \
                               cursor-pointer";
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
                form
                  ~a:[ a_id "delete-form" ]
                  [
                    table
                      ~a:
                        [
                          a_class
                            [
                              "bg-white shadow-md rounded divide-y \
                               divide-gray-200 overflow-hidden w-full";
                            ];
                        ]
                      ~thead:
                        (thead
                           [
                             tr
                               [
                                 th [ txt "Blocked domain" ];
                                 th [ (* the delete button *) ];
                               ];
                           ])
                      (List.map blocked_row manual_blocked_domains);
                  ];
                table
                  ~a:
                    [
                      a_class
                        [
                          "bg-white shadow-md rounded divide-y divide-gray-200 \
                           overflow-hidden w-full";
                        ];
                    ]
                  ~thead:
                    (thead
                       [
                         tr
                           [
                             th [ txt "Block list" ];
                             th [ txt "Number of blocked domains" ];
                           ];
                       ])
                  (SM.fold
                     (fun list cnt acc ->
                       tr
                         [
                           td [ a ~a:[ a_href list ] [ txt list ] ];
                           td [ txt (string_of_int cnt) ];
                         ]
                       :: acc)
                     lists []);
              ];
          ];
      ])
