open Eliom_content
open Eliom_lib

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

  let make: 'a params_type -> ('a -> unit Lwt.t) -> Widgets.div_content =
    fun params callback ->
      let rec build_form: type a. a params_type -> Widgets.div_content = function
        | TAtom(name, default, TString) ->
          Html5.F.(Raw.input ~a:[a_input_type `Text] ())
        | TAtom(name, default, TInt) ->
          Html5.F.(Raw.input ~a:[a_input_type `Text] ())
        | TProd(a, b) -> 
          Html5.F.div [build_form a; build_form b]
      in
      build_form params
      

end
