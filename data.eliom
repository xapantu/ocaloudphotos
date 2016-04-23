module Data (Config:App_stub.CONFIG) : App_stub.DATA = struct
	type volume = string

	let all_volumes () =
		["california"; "low"]
	
	let volumes_enabled_for name =
		match name with
		| "photos" -> ["california"]
		| "files" -> ["low"]
	
	let from_id n = n
	
	let volume_id n = n

	let volume_path n = Sys.getcwd () ^ "/data/" ^ n ^ "/"

	let notify_volumes = ref []

	let public_volume n = true

	let notify_on_new_volume f =
		notify_volumes := f::!notify_volumes;
		List.iter f @@ all_volumes ()
end
