[%%shared open Lwt
open Eliom_lib
open Eliom_content
]


module Testnodes_app =
  Eliom_registration.App (
    struct
	    let application_name = "testnodes"
		  end)

module Files(E:App_stub.ENV) : App_stub.FILES with type volume = E.Data.volume = struct
	
	open E.Data

	type volume = E.Data.volume

	exception Volume_not_found

	let serv_volume = Hashtbl.create 10
	
	let main_service2 =
	  Eliom_service.App.service ~path:["c"] ~get_params:Eliom_parameter.(suffix @@ string "volume") ()

	let () = E.Config.App.register
		~service:main_service2
		(fun (volume_id) () ->
			let open Html5.F in
			E.Data.Devices.new_device volume_id;
			Lwt.return (Eliom_tools.F.html ~title:"test" (body [])))
    
	let main_service =
	  Eliom_service.App.service ~path:["v"] ~get_params:Eliom_parameter.(suffix @@ string "volume") ()

	let () = E.Config.App.register
		~service:main_service
		(fun (volume_id) () ->
			try%lwt
				let volume = E.Data.from_id volume_id in
				let open E.Data in
				let ul_content =
					volume_list_files volume
					|> List.map (fun s -> Html5.F.li [Html5.F.pcdata s])
				in
				let file_list = Html5.F.ul ul_content in

				let ev = Eliom_react.S.Down.of_react Devices.all_devices in
				
				let ul = [%client
					React.S.map  (fun l ->
					List.map (fun l -> Html5.F.(li [pcdata l])) l) ~%ev
					|>
					 React.S.map (fun l -> Html5.F.ul l)
					 |> Html5.R.node]
					in
				Lwt.return
				(Eliom_tools.F.html
					 ~title:"files"
					 ~css:[["css";"ocaloudphotos.css"]]
					 (Html5.F.body 
					 [file_list; Html5.C.node ul; Html5.F.pcdata "test"]
					 ))
			with
			| Volume_not_found -> return
				(Eliom_tools.F.html
					 ~title:"ocaloudphotos"
					 ~css:[["css";"ocaloudphotos.css"]]
					 Html5.F.(body [
					 p [pcdata "This volume does not seem to exist."];
					 ]))
		)
	
	let register_service_for_volume n =
	  Eliom_service.preapply ~service:main_service (E.Data.volume_id n)
	  |> E.Mimes.register_public ("volume: " ^ E.Data.volume_id n)

			  
	let on_new_volume vol =
		if public_volume vol then
			(* FIXME: Check if this path is available. This service needs to be saved in
			 * order to be deleted afterwards. *)
			  let service = Eliom_service.App.service
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
			  ;
		register_service_for_volume vol
	
	let download_path vol filename =
		[filename]
	
	let service_for_volume n =
		Hashtbl.find serv_volume n
	
	let () =
		React.E.map on_new_volume E.Data.new_volume_loaded; ()
	
end
