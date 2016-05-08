let xdr_options = Xdr.(string "key" **
					string "value")

let xdr_devices = Xdr.(string "id" **
					string "name" **
					int "address_count" **
					string "address" **
					int "compression" **
					string "cert_name" **
					int64 "max_local_version" **
					int "flags" **
					list xdr_options "options")

let xdr_folders = Xdr.((string "id" **
					list xdr_devices "devices") ** (
					int "flags" **
					list xdr_options "options"))

let xdr_cluster_config = Xdr.(string "device_name" **
					string "client_name" **
					string "client_version" **
					list xdr_folders "folders" ** 
					list xdr_options "options")

let xdr_message = Xdr.(flag 4 "bep_version" ** 
					flag 12 "message_id" **
					flag  8 "message_type" **
					flag 7 "reserved" **
					bool "compression" **
					string "message")

let xdr_message_header = Xdr.(flag 4 "bep_version" ** 
					flag 12 "message_id" **
					flag  8 "message_type" **
					flag 7 "reserved" **
					bool "compression" **
					int "message_length")


let xdr_counters = Xdr.(int64 "deviceid" ** int64 "value")

let xdr_blocks = Xdr.(int "size" ** string "hash")

let xdr_file_info = Xdr.(string "name" **
					(flag 14 "reserved" ** bool "symbolic_link_target_not_exists" ** bool "symbolic_link" ** bool "no_permissions" ** bool "directory" ** bool "invalid" ** bool "deleted" ** flag 12 "unix") **
					int64 "modified" **
					list xdr_counters "counters" **
					int64 "value" (* local version *) **
					list xdr_blocks "blocks")

let xdr_index = Xdr.(string "folder" **
					list xdr_file_info "files" **
					int "flags" **
					list xdr_options "options")

let xdr_request = Xdr.(string "folder" **
					string "name" **
					int64 "offset" **
					int "size" **
					string "hash" **
					int "flags" **
					list xdr_options "options")

let xdr_ping = Xdr.(string "reason" ** int "code")

let xdr_data = Xdr.(string "data" ** int "code")
