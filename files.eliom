open Lwt

module Files(E:App_stub.ENV) : App_stub.FILES with type volume = E.Data.volume = struct
	
	open E.Data

	type volume = E.Data.volume

	let serv_volume = Hashtbl.create 10
			  
	let on_new_volume vol =
		if public_volume vol then
			(* FIXME: Check if this path is available. This service needs to be saved in
			 * order to be deleted afterwards. *)
			  let service = Eliom_service.Http.service
			    ~path:["f";volume_id vol]
				~get_params:Eliom_parameter.(suffix (all_suffix "path"))
				()
			  in
			  Eliom_registration.File.register
			    ~service
				(fun s () ->
				  let s = Ocsigen_lib_base.Url_base.remove_dotdot s in
				  return (volume_path vol ^ Ocsigen_lib.Url.string_of_url_path ~encode:false s));
			  Hashtbl.add serv_volume vol service
	
	let download_path vol filename =
		[filename]
	
	let service_for_volume n =
		Hashtbl.find serv_volume n
	
	let () =
		E.Data.notify_on_new_volume on_new_volume
end
