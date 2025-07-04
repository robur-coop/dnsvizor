let login_page =
  Tyxml_html.(
    main
      ~a:
        [
          a_class
            [
              "w-full text-gray-900 min-h-screen flex items-center \
               justify-center bg-gray-50";
            ];
        ]
      [
        section
          ~a:[ a_class [ "w-full max-w-md p-8 bg-white shadow-md rounded-lg" ] ]
          [
            h1
              ~a:
                [
                  a_class
                    [ "text-2xl font-bold text-center text-cyan-800 mb-6" ];
                ]
              [ txt "Welcome to DNSvizor" ];
            p
              ~a:[ a_class [ "my-3" ] ]
              [ txt "Please authenticate yourself to access this feature." ];
            form
              ~a:
                [
                  a_action "/login";
                  a_method `Post;
                  a_enctype "multipart/form-data";
                  a_class [ "space-y-6" ];
                ]
              [
                div
                  [
                    label
                      ~a:
                        [
                          a_class
                            [ "block text-sm font-medium text-gray-700 mb-2" ];
                          a_label_for "password";
                        ]
                      [ txt "Password" ];
                    input
                      ~a:
                        [
                          a_input_type `Password;
                          a_id "password";
                          a_name "password";
                          a_required ();
                          a_class
                            [
                              "block w-full p-3 border border-gray-300 \
                               rounded-lg bg-gray-50 text-gray-800 \
                               focus:outline-none focus:border-blue-500 \
                               focus:ring-1 focus:ring-blue-500";
                            ];
                        ]
                      ();
                  ];
                div
                  ~a:[ a_class [ "flex justify-end" ] ]
                  [
                    button
                      ~a:
                        [
                          a_button_type `Submit;
                          a_class
                            [
                              "px-6 py-2 bg-cyan-600 text-white rounded-md \
                               hover:bg-cyan-700 focus:outline-none \
                               focus:ring-2 focus:ring-cyan-500 \
                               focus:ring-opacity-50";
                            ];
                        ]
                      [ txt "Login" ];
                  ];
              ];
          ];
      ])
