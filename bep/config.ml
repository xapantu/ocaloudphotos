let client = "ocaloud"

let client_version = "0.1"

let can_compress = false

let bep_version = 0

type message_type =
	| ClusterConfig
	| Index
	| Request
	| Response
	| Ping
	| Unknown
	| IndexUpdate
	| Close

let type_to_int = function
	| ClusterConfig -> 0
	| Index -> 1
	| Request -> 2
	| Response -> 3
	| Ping -> 4
	| Unknown -> 5
	| IndexUpdate -> 6
	| Close -> 7

let type_from_int = function
	| 0 -> ClusterConfig
	| 1 -> Index
	| 2 -> Request
	| 3 -> Response
	| 4 -> Ping
	| 6 -> IndexUpdate
	| 7 -> Close
	| _ -> failwith "Unknown message type"

let name = "ocamlexperiment"

(* config dir, it must be private *)

let config_dir =
	let dir = (Unix.getenv "HOME") ^ "/.config/ocaloud/" in
	Utils.Fileutils.ensure_path dir; dir
