open Tyxml

let header ?(page_title = "DNSVizor") () =
  Html.(
    head
      (title (txt page_title))
      [
        meta ~a:[ a_charset "UTF-8" ] ();
        meta
          ~a:
            [
              a_name "viewport";
              a_content "width=device-width, initial-scale=1.0";
            ]
          ();
        script ~a:[ a_src "/main.js" ] (txt "");
        link ~rel:[ `Stylesheet ] ~href:"/style.css" ();
        script ~a:[ a_src "https://kit.fontawesome.com/d1697f2fa9.js" ] (txt "");
        (* TODO: change from the taildwind cdn to a local css file *)
        script
          ~a:[ a_src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ]
          (txt "");
      ])
