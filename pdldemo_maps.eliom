
[%%shared
   open Eliom_content.Html.F
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


(* Page for this demo *)
let%shared page () =
  Lwt.return Eliom_content.Html.[
    F.h1 [pcdata "Demo Maps"]
  ]
