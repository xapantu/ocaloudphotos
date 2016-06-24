open Eliom_content
open Eliom_lib
[%%client
(*  open Html5.F*)
]

type form_content = Html5_types.form_content_fun Eliom_content.Html5.elt

module Form(Data: App_stub.DATA) = struct
  type _ atom =
    | TInt			: int atom
    | TBool    	  	: bool atom
    | TString   		: string atom
    | TStringPassword	: string atom

  type _ params_type =
    | TProd : ( 'a params_type * 'b params_type) -> ('a * 'b) params_type
    | TAtom : (string * 'a * 'a atom) -> 'a params_type

  let int (n : string) def = TAtom (n,def, TInt)

  let bool (n : string) def = TAtom (n, def, TBool)

  let string (n : string) def = TAtom (n, def, TString)
  let string_password (n : string) def = TAtom (n,def, TStringPassword)

  let prod t1 t2 = TProd (t1,t2)

  let ( ** ) = prod

  let service_stub param func =
    Eliom_registration.Ocaml.register_post_coservice'
      ~post_params:param
      (fun () p -> func p)

  let client_prod: (unit -> 'a) Eliom_lib.client_value -> (unit -> 'b) Eliom_lib.client_value -> (unit -> ('a * 'b)) Eliom_lib.client_value =
    fun a b ->
      Obj.magic ([%client Obj.magic (fun () ->
           ~%a (), ~%b ())])

  let make: 'a params_type -> ('a -> unit Lwt.t) -> Widgets.div_content =
    fun params callback ->
      (*let eliom_params = to_eliom params in
      let coservice = service_stub eliom_params callback in*)
      let rec build_form: type a. a params_type -> form_content * ((unit -> a) Eliom_lib.client_value) = function
        | TAtom(name, default, TString) ->
          let i = Html5.F.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            (fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              (i##.value) : unit -> string)] : (unit -> string) Eliom_lib.client_value)
        | TAtom(name, default, TInt) ->
          let i = Html5.F.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              int_of_string (Js.to_string i##.value)] : (unit -> int) Eliom_lib.client_value)
        | TProd(a, b) -> 
          let a, fa = build_form a
          and b, fb = build_form b in
          let fa = Obj.magic fa in
          let fb = Obj.magic fb in
          Html5.F.div [a; b], Obj.magic (client_prod fa fb)
      in
        Html5.F.div [Html5.F.(Raw.form [fst @@ build_form params; Raw.input ~a:[a_input_type `Submit] ()])]
      

end
