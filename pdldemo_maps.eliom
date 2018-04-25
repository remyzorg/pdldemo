
[%%client
   open Eliom_content.Html
]



[%%shared
  open Eliom_content.Html.D
  module Option = Base.Option
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



[%%client
open Googlemaps

let create_map map =
  let opts = MapOptions.create ~zoom:15 () in
  let m = Map.new_map (To_dom.of_div map) ~opts () in
  m

let update_location map new_location =
  Firebug.console##debug map;
  Firebug.console##debug new_location;
  let center = LatLng.new_lat_lng
      ~lat:(fst new_location) ~lng:(snd new_location)
  in
  Option.iter (map) ~f:(fun m ->
    Map.set_center m center)

let%sync p ~animate page_loading new_location =
  let maps_already_loaded = () in
  let maps_loading = () in
  let map = None in

  loop (
    !(print_endline "Trigger p");
    present new_location !(print_endline "new_location")
  )
  ||
  loop (
    (await map || await new_location);
    !(update_location !!map !!new_location);
    loop (
      present new_location
        !(update_location !!map !!new_location)
    )
    ; pause
  )
  ||
  loop (present page_loading (present maps_already_loaded (
    !(print_endline "We don't load Maps again"))
    (!(onmapsload (fun () ->
       Pendulum.Signal.set_present_value maps_loading ()
     ; animate () )));
   pause))
  ||
  loop ( present maps_loading (
    !(print_endline "Maps loaded")
  ; emit map (Some (create_map (!!page_loading)))
  ; pause
  ; loop (
    present page_loading (
      emit map (Some (create_map (!!page_loading))) ;
      emit maps_already_loaded;)
  ; pause)))

let p = p#create (div [], (0., 0.))

]


(* Page for this demo *)
let%shared page () =
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
    Lwt_js_events.async (fun () ->
      p#page_loading ~%map;
      p#react;

      if Geolocation.is_supported() then begin
        let geo = Geolocation.geolocation in
        let options = Geolocation.empty_position_options () in
        options##.enableHighAccuracy := true;
        let f_success e =
          p#new_location (e##.coords##.latitude, e##.coords##.longitude);
          p#react
        in
        let f_error e = Firebug.console##debug e in
        (* geo##getCurrentPosition (Js.wrap_callback f_success) (Js.wrap_callback f_error) options; *)
        ignore @@
        geo##watchPosition (Js.wrap_callback f_success) (Js.wrap_callback f_error) options;
      end;
      Lwt.return ()
    )

  ] in
  Lwt.return
    [ marker_input
    ; map
    ]

