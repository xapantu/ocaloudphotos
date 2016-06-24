open Eliom_content
open Eliom_lib
    [%%shared
        type 'a cocaml = (unit, 'a, Eliom_service.service_method,
                                 Eliom_service.attached,
                                 Eliom_service.service_kind,
                                 [ `WithoutSuffix ], unit, string,
                                 Eliom_service.registrable,
                                 unit Eliom_service.ocaml_service) Eliom_service.service
    ]
[%%client
    open Lwt
let get_from_server service param = Eliom_client.call_ocaml_service ~service () param
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
      ([%client (fun () ->
           ~%a (), ~%b ())])

  (* too much magic in that… *)
  let rec to_eliom: type a c. a params_type -> (a, 'b, c) Eliom_parameter.params_type = function
    | TAtom(n, _, TString) -> Obj.magic @@ Eliom_parameter.string n
    | TAtom(n, _, TInt) -> Obj.magic @@ Eliom_parameter.int n
    | TProd(a, b) -> Obj.magic @@ Eliom_parameter.(to_eliom a ** to_eliom b)

  let rec make_default: type a. a params_type -> a = function
    | TAtom(_, default, _) -> default
    | TProd(a, b) -> make_default a, make_default b

  let make: 'a params_type -> ('a -> unit Lwt.t) -> Widgets.div_content =
    fun params callback ->
      let eliom_params = to_eliom params in
      let coservice: 'a cocaml = service_stub eliom_params callback in
      let rec build_form: type a. a params_type -> form_content * ((unit -> a) Eliom_lib.client_value) = function
        | TAtom(name, default, TString) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            (fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              Js.to_string i##.value)] : (unit -> string) Eliom_lib.client_value)
        | TAtom(name, default, TInt) ->
          let i = Html5.D.(Raw.input ~a:[a_input_type `Text; a_placeholder name] ()) in
          i, ([%client
            fun () ->
              let i = Eliom_content.Html5.To_dom.of_input ~%i in
              Js.Unsafe.eval_string "alert(42)";
              int_of_string @@ Js.to_string i##.value] : (unit -> int) Eliom_lib.client_value)
        | TProd(a, b) -> 
          mk_prod a b
          (* too much magic in there, anyone got any idea for that? *)
      and
        mk_prod: type a b. (a params_type -> b params_type -> form_content * ((unit -> a * b) Eliom_lib.client_value)) = fun a b ->
          let a, fa = build_form a
          and b, fb = build_form b in
          Html5.F.div [a; b], Obj.magic (client_prod (Obj.magic fa) (Obj.magic fb))
      in
      let elt, (cb:(unit -> 'a) Eliom_lib.client_value) = build_form params in
      let default = make_default params in
      let cb = [%client fun () ->
           ~%cb ()] in
      let full_cb = [%client fun e ->
            Dom.preventDefault e;

            let _ = get_from_server (~%coservice:string cocaml) (~%cb ()) in ()
      ] in
      Html5.F.div [Html5.F.(Raw.form ~a:[a_onsubmit full_cb] [elt; Raw.input ~a:[a_input_type `Submit] ()])]
      

end
