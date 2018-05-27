
[%%client
   open Eliom_content.Html
]



[%%shared
  open Eliom_content.Html.D
  (* module Option = BatOption *)
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

let%client start_watching_loc each_location =
  if Geolocation.is_supported() then begin
    let geo = Geolocation.geolocation in
    let options = Geolocation.empty_position_options () in
    options##.enableHighAccuracy := true;
    let f_error e = Firebug.console##debug e in
    let f_success e = each_location (e##.coords##.latitude, e##.coords##.longitude) in
    geo##getCurrentPosition (Js.wrap_callback f_success) (Js.wrap_callback f_error) options;
    ignore @@ geo##watchPosition (Js.wrap_callback f_success) (Js.wrap_callback f_error) options;
  end



[%%client
  open Googlemaps

(*
48.8455669,2.3541834
*)

  let create_mymarker map (lat,lng) =
    let position = LatLng.new_lat_lng ~lat ~lng in
    let opts = MarkerOptions.create
        ~position
        ~title:("Mon point")
        ~draggable:false
        ~map:map () in
    Marker.new_marker ~opts ()

  let create_map (lat,lng) map =
    let opts = MapOptions.create ~zoom:15 () in
    let center = LatLng.new_lat_lng
        ~lat ~lng
    in
    let m = Map.new_map (To_dom.of_div map) ~opts () in
    Map.set_center m center;
    m

  let update_location map marker (lat,lng) =
    Firebug.console##debug (lat,lng);
    let center = LatLng.new_lat_lng
        ~lat ~lng
    in
    Marker.set_position marker center;
    Map.set_center map center

  let init_searchbox elt f =
    let inp = To_dom.of_element elt in
    let sb = SearchBox.new_search_box inp () in
    ignore @@ Event.add_listener (SearchBox.t_to_js sb) "places_changed" (fun _ ->
      f @@ SearchBox.get_places sb
      );
    sb

  let remove_markers ms =
    List.iter (fun m -> Marker.set_map m None) ms

  let generate_markers map places =
    let bds, markers = List.fold_left (fun (b, mrks) p ->
      let position = PlaceGeometry.location @@ PlaceResult.geometry p in
      let bounds = match b with
      | None -> Some (LatLngBounds.new_lat_lng_bounds ~sw:position ~ne:position)
      | Some b -> Some (LatLngBounds.extend b position)
      in
      let opts = MarkerOptions.create
          ~position
          ~title:(PlaceResult.name p)
          ~map ()
      in bounds, Marker.new_marker ~opts () :: mrks
    ) (None, []) places
    in
    Eliom_lib.Option.iter (Map.fit_bounds map) bds;
    Map.set_zoom map (min (Map.get_zoom map) 16);
    markers

]

type%client page_load = {
  map : Html_types.div Eliom_content.Html.D.elt;
  search_input : Html_types.input Eliom_content.Html.D.elt;
  locate_btn : Html_types.button Eliom_content.Html.D.elt;
}


(*
  let%sync p ~animate =
    input (page_loading : page_load);

    let location = 0.,0. in
    let maps_loading = () in
    begin
      !(onmapsload (fun () ->
        Pendulum.Signal.set_present_value maps_loading ()
      ; animate () ))
    ; !(start_watching_loc (fun loc ->
        Pendulum.Signal.set_present_value location loc
      ; animate ()))
    ; !()
    ; (await page_loading || await maps_loading || await location)
    ; !(print_endline "Maps loaded")
    ; loop ( trap reload (
      let map = create_map !!location (!!page_loading).map in
      let mymarker = create_mymarker !!map !!location in
      let places_changed = [] in
      let searchbox = init_searchbox (!!page_loading).search_input
          (fun places ->
             Pendulum.Signal.set_present_value places_changed places ;
             animate ()) in
      let locate = () in
      let searching = () in
      let markers = [] in
      let locate = () in
         loop ( present page_loading ( exit reload ))
      || loop ( present places_changed (
                !(remove_markers !!markers);
                emit markers (generate_markers !!map !!places_changed)))
      || loop ( present places_changed (
                trap t (loop (present locate (exit t); emit searching;))))
      || loop ( present location (
                present searching nothing
                  !(update_location !!map !!mymarker !!location)
      ))
    ))
*)


[%%client

  let%sync p ~animate =
    input (page_loading : page_load);

    let location = 0.,0. in
    let maps_loading = () in

    begin
      !(onmapsload (fun () ->
        Pendulum.Signal.set_present_value maps_loading ()
      ; animate () ))
    ; !(start_watching_loc (fun loc ->
        Pendulum.Signal.set_present_value location loc
      ; animate ()))
    ; !()

    ; (await page_loading || await maps_loading || await location)
    ; !(print_endline "Maps loaded")
    ; let map = create_map !!location (!!page_loading).map in
      let mymarker = create_mymarker !!map !!location in
      let places_changed = [] in
      let searchbox = init_searchbox (!!page_loading).search_input (fun places ->
        Pendulum.Signal.set_present_value places_changed places ;
        animate ())
      in
      let locate = () in
      !((To_dom.of_button (!!page_loading).locate_btn)##.onclick :=
          Dom_html.handler (fun _ ->
            Pendulum.Signal.set_present_value locate ();
            animate (); Js._true
          ));
      loop ( present page_loading (
        !((To_dom.of_button (!!page_loading).locate_btn)##.onclick :=
            Dom_html.handler (fun _ ->
              Pendulum.Signal.set_present_value locate ();
              animate (); Js._true
            ));
        emit map (create_map !!location (!!page_loading).map));
        emit mymarker (create_mymarker !!map !!location);
        emit searchbox (init_searchbox (!!page_loading).search_input (fun places ->
          Pendulum.Signal.set_present_value places_changed places;
          animate ()))
      )
      ||
      let searching = () in
      let markers = [] in
      loop ( present places_changed (
        !(remove_markers !!markers);
        emit markers (generate_markers !!map !!places_changed)))
      ||
      loop ( present places_changed (
        trap t (loop (present locate (exit t); emit searching;))))
      ||
      loop ( present location (
        present searching nothing (
          !(update_location !!map !!mymarker !!location)
        )
      ))
      ||
      loop ( present locate ( emit location !!location); pause)
    end

]



let%client p =
  p#create ({map = div []; search_input = input ();
            locate_btn = button []})



(* Page for this demo *)
let%shared page () =

  let search_input =
    Eliom_content.Html.D.input
      ~a:[a_class ["pac-input"];
          a_input_type `Text; a_placeholder "Search..."] ()
  in

  let map = Eliom_content.Html.D.div ~a:[a_id "map"; a_class ["map"]] [] in
  let locate_btn = button [pcdata "Locate"] in

  let marker_text_input = input ~a:[ a_input_type `Text ] () in
  let marker_btn_input =
    button ~a:[ a_class [ "marker-btn-input" ]][ pcdata "Search" ]
  in
  let marker_btn_cancel =
    button ~a:[ a_class [ "marker-btn-cancel" ]][ pcdata "Search" ]
  in
  let marker_input =
    div ~a:[ a_id "marker-input"; a_class [ "marker-input" ] ]
      [ marker_text_input
      ; marker_btn_input
      ; marker_btn_cancel
      ]
  in
  let page = div
      [ marker_input
      ; div [search_input; locate_btn]
      ; map
      ]
  in

  let _ : unit Eliom_client_value.t = [%client
    Lwt_js_events.async (fun () ->
      p#page_loading ({map = ~%map; search_input = ~%search_input;
                      locate_btn = ~%locate_btn });
      p#react;
      Lwt.return ()
    )

  ] in
  Lwt.return [ page ]
