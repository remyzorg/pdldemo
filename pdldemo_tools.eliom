[%%shared.start]

module type Page = sig

  val name : unit -> string

  val page_class : string

  val service :
    (unit, unit,
     Eliom_service.get,
     Eliom_service.att,
     Eliom_service.non_co,
     Eliom_service.non_ext,
     Eliom_service.reg,
     [ `WithoutSuffix ],
     unit, unit,
     Eliom_service.non_ocaml)
      Eliom_service.t

  val page :
    unit ->
    (Html_types.div_content Eliom_content.Html.D.elt) list Lwt.t

end

let demos =
  [ (module Pdldemo_maps : Page)
  (* ; (module Pdldemo_todomvc) *)
  ]

let drawer_contents () =
  let open Eliom_content.Html.F in
  let make_link (module D : Page) =
    li [ a ~service:D.service [pcdata @@ D.name ()] () ]
  in
  let submenu =
    ul ~a:[a_class ["os-drawer-submenu"]] (List.map make_link demos)
  in
  li [ a ~a:[ a_class ["os-drawer-item"] ]
         ~service:Pdldemo_services.demo_service
         [%i18n demo_intro] ()
     ; submenu ]
