
(* [%%shared
 *    open Eliom_content.Html.F
 * ] *)

[%%client
  open Eliom_content.Html
]

let%server service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["pdldemo-maps"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()


(* Make service available on the client *)
let%client service = ~%service


let%shared name () = "Maps"

(* Class for the page containing this demo (for internal use) *)
let%shared page_class = "os-page-demo-rpc"

let%client onmapsload f =
  let callback = Js.wrap_callback f in
  let callback_name = "googlemapsloaded" in
  Js.export callback_name callback ;
  let script = Dom_html.createScript Dom_html.document in
  script##.defer := Js._true ;
  script##.async := Js._true ;
  script##._type := Js.string "text/javascript";
  let key = !Pdldemo_config.googlemaps_api_key in
  print_endline key;
  script##.src :=
    Js.string @@
    "https://maps.googleapis.com/maps/api/js?\
     key=" ^ key ^ "\
             &libraries=geometry,drawing,places\
             &callback=" ^ callback_name ;
  Dom.appendChild Dom_html.document##.body script


(* Page for this demo *)
let%shared page () =
  let open Eliom_content.Html.D in
  let marker_text_input = input ~a:[ a_input_type `Text ] () in
  let marker_btn_input =
    button ~a:[ a_class [ "marker-btn-input" ]][ pcdata "Search" ]
  in
  let marker_btn_cancel =
    button ~a:[ a_class [ "marker-btn-cancel" ]][ pcdata "Search" ]
  in
  let map = div ~a:[a_id "map"; a_class ["map"]] [] in
  let marker_input =
    div ~a:[ a_id "marker-input"; a_class [ "marker-input" ] ]
      [ marker_text_input
      ; marker_btn_input
      ; marker_btn_cancel
      ]
  in
  let _ : unit Eliom_client_value.t = [%client
    let open Googlemaps in
    Lwt_js_events.async (fun () ->
      print_endline "Hello";
      onmapsload (fun () ->

        (* (To_dom.of_div ~%map)##.innerHTML := Js.string "lol"; *)

        (* print_endline "Ok"; *)
        let position = LatLng.new_lat_lng ~lat:(-34.397) ~lng:150.644 in

        let opts = MapOptions.create ~center:position ~zoom:10 () in
        let () = MapOptions.set_zoom opts 8 in
        let _map = Map.new_map (To_dom.of_div ~%map) ~opts () in
        ()
      );
      Lwt.return ()
    )

  ] in
  Lwt.return
    [ marker_input
    ; map
    ]

