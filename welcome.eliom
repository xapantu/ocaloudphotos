open Lwt
open Eliom_lib
open Eliom_content
open Html5
open F

module Welcome(E:App_stub.ENV) = struct
	let main_service =
	  Eliom_service.Http.service ~path:[] ~get_params:Eliom_parameter.unit ()

    let () = Eliom_registration.Html5.register
		~service:main_service
		(fun () () ->
			let all_public = E.Mimes.get_all_public_services ()
			|> List.map (fun (name, service) ->
				a service [pcdata name] ()
			) in
			return
			(Eliom_tools.F.html
				 ~title:"ocaloud"
				 ~css:[["css";"ocaloud.css"]]
				 Html5.F.(body (
				 all_public
				 )))
		)
end
