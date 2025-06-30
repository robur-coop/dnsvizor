let configuration_page =
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
                  [ txt "DNSmasq Configuration Upload" ];
                p
                  ~a:[ a_class [ "mb-6 text-gray-700" ] ]
                  [
                    txt
                      "Upload your complete DNSmasq configuration file (.conf) \
                       here. All DNSmasq options are managed directly within \
                       this file.";
                  ];
                form
                  ~a:
                    [
                      a_action "/configuration/upload";
                      a_method `Post;
                      a_enctype "multipart/form-data";
                      a_class [ "bg-white p-6 rounded-lg shadow-md" ];
                    ]
                  [
                    div
                      ~a:[ a_class [ "mb-4" ] ]
                      [
                        label
                          ~a:
                            [
                              a_class
                                [
                                  "block text-sm font-medium text-gray-700 mb-2";
                                ];
                            ]
                          [ txt "Select DNSmasq Configuration File:" ];
                        (* Hidden file input *)
                        input
                          ~a:
                            [
                              a_input_type `File;
                              a_id "dnsmasq_config_file";
                              a_name "dnsmasq_config_file";
                              a_class [ "hidden" ];
                              a_accept [ ".conf"; ".txt" ];
                            ]
                          ();
                        div
                          ~a:
                            [
                              a_onclick
                                "document.getElementById('dnsmasq_config_file').click()";
                              (* JavaScript to click the hidden input *)
                              a_class
                                [
                                  "inline-block px-4 py-2 bg-gray-200 \
                                   text-gray-800 rounded-md hover:bg-gray-300 \
                                   focus:outline-none focus:ring-2 \
                                   focus:ring-gray-400 focus:ring-opacity-75 \
                                   cursor-pointer transition-colors \
                                   duration-200";
                                ];
                            ]
                          [ txt "Choose File" ];
                        span
                          ~a:
                            [
                              a_id "selected_file_name";
                              a_class [ "ml-3 text-gray-600 italic" ];
                            ]
                          [ txt "No file chosen" ];
                      ];
                    div
                      ~a:[ a_class [ "mb-6" ] ]
                      [
                        label
                          ~a:
                            [
                              a_class
                                [
                                  "block text-sm font-medium text-gray-700 mb-2";
                                ];
                            ]
                          [ txt "DNSmasq config preview:" ];
                        textarea
                          ~a:
                            [
                              a_id "file_content_display";
                              a_name "file_content_preview";
                              a_rows 15;
                              a_class
                                [
                                  "block w-full p-3 border border-gray-300 \
                                   rounded-lg bg-gray-50 text-gray-800 \
                                   font-mono text-sm resize-y \
                                   focus:outline-none focus:border-blue-500 \
                                   focus:ring-1 focus:ring-blue-500";
                                ];
                              a_readonly ();
                              (* Make it read-only for preview *)
                            ]
                          (txt "");
                      ];
                    div
                      ~a:[ a_class [ "flex justify-end" ] ]
                      [
                        button
                          ~a:
                            [
                              a_class
                                [
                                  "px-6 py-2 bg-cyan-600 text-white rounded-md \
                                   hover:bg-cyan-700 focus:outline-none \
                                   focus:ring-2 focus:ring-cyan-500 \
                                   focus:ring-opacity-50";
                                ];
                            ]
                          [ txt "Upload Configuration" ];
                      ];
                  ];
              ];
          ];
      ])
