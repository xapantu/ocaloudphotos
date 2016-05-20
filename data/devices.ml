open Eliom_shared.React

module Devices = struct
	type device = string

	let list_devices () =
		["xaptop"]

	let new_device, send_device = React.E.create ()

	let () = send_device "xaptop"

	let all_devices, send_device = React.S.create ["testlolo"]
	let () = send_device ["testttt"]

	let name n = n

	let new_device n = send_device (n :: React.S.value all_devices)
end
