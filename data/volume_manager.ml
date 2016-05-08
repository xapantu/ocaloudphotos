open React

module VolumeManager (Config:App_stub.CONFIG) : App_stub.DATA = struct
	type volume = string
	
	let volumes = Hashtbl.create 10
	
	let new_volume_loaded, send_vol = E.create ()

	let all_volumes () =
		Hashtbl.fold (fun _ a l -> a::l) volumes []
	
	let volumes_enabled_for name =
		match name with
		| "photos" -> ["california"]
		| "files" -> ["low"]
	
	let from_id n = n
	
	let volume_id n = n

	let volume_path n = Sys.getcwd () ^ "/data2/" ^ n ^ "/"

	let public_volume n = true

	let load_volumes () =
		Hashtbl.add volumes "california" "california";
		send_vol "california";
		Hashtbl.add volumes "low" "low";
		send_vol "low"



end
