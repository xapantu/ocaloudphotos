open Lwt
open Protocol

let init_ssl () =
	Ssl_threads.init ();
	return (Ssl.init ())

let connect_to hostname port () =
	let he = Unix.gethostbyname hostname in
	let socketaddr = Unix.ADDR_INET(he.h_addr_list.(0), port) in
	let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
	Ssl.use_certificate ctx "cert.pem" "key.pem";
	let socket = Ssl.open_connection_with_context ctx socketaddr in
	let cert = Ssl.get_certificate socket in
	print_string @@ Security.get_id_from_certificate cert;
	return socket

let match_cluster_config bs =
	let bs, (
		device_name, (
		client_name, (
		client_version, (
		folders, (
		options))))) =
		Xdr.(read_bitstring xdr_cluster_config  bs) in
		
		List.iter (fun f ->
			let ((id, devices), (flags, options_number)) = f in
			Printf.printf "%s shared with %d devices\n" id (List.length devices);
			List.iter (fun d -> let id, (name, _) = d in Printf.printf "%s: %s\n" (id) name) devices;) folders;
		
		Printf.printf "device_name : %s\n" device_name;
		Printf.printf "client_name : %s\n" client_name;
		Printf.printf "client_version : %s\n" @@ client_version;
		Printf.printf "folder_structure : %d\n" @@ List.length folders;
		Printf.printf "option for this device : %d\n" @@ List.length options;
		bs, folders


let build_config_message () =
	Xdr.to_string xdr_cluster_config (Config.name,
									 (Config.client,
									 (Config.client_version,
									 (List.map (Share.to_xdr @@ Devicecontext.get_my_device ()) (Devicecontext.get_shares ()),
									 []))))

let build_index_message () =
	let f = List.hd (Devicecontext.get_shares ()) in
	let name = Share.name f in
	let%lwt files = Share.files f in
	List.iter(fun (name, (_, (_, (_, (_, a))))) -> Format.printf "sending %s %d@." name  (List.length a)) files;
	return @@ Xdr.to_string xdr_index (name, (files, (0, [])))

let build_message_wrapper message message_id message_type =
	(*print_endline "message sent";
	print_endline message;*)
	Xdr.to_string xdr_message (Config.bep_version,
					(message_id,
					(Config.type_to_int message_type,
					(0,
					(Config.can_compress,
					message)))))

let send_wrapped_message socket msg =
	let str = Bitstring.string_of_bitstring msg in
	Ssl.output_string socket str;
	Ssl.flush socket; return ()

let match_index bs =
	let bs, data = Xdr.read_bitstring xdr_index bs in
	let (name, (files, _))  = data in 
	data

let current_id = ref 1

let id () =
	incr current_id; !current_id

let send_greetings socket = 
	let message = build_config_message () in
	let wrapped_message = build_message_wrapper (Bitstring.string_of_bitstring  message) (id()) ClusterConfig in
	send_wrapped_message socket wrapped_message;
	Lwt.return socket
				

let send_bitstring socket bs message_type =
	let myid = id () in
	let wrapped_message = build_message_wrapper (Bitstring.string_of_bitstring bs) myid message_type in
	send_wrapped_message socket wrapped_message; myid

let do_job socket = function
	| Jobs.FileRetrieve (folder, file, offset, size, cb) ->
		begin
		Printf.printf "Retrieving file %s of folder %s from %d with length %d\n" file folder (Int64.to_int offset) size;

		let request = Xdr.to_string xdr_request (folder, (file, (offset, (size, ("", (0, [])))))) in
		
		let id = send_bitstring socket request Request in
		Jobs.add_callback id cb;
		end
	| Jobs.CopyData(wakeup) ->
		let _ = Lwt_main.yield () >>= (fun () -> Lwt.wakeup wakeup ("", 0); Lwt.return ()) in
		()
	| Jobs.EmptyJob -> ()

(* Lwt friendly ssl reader *)
let read_blocking socket count =
	let message_bytes = String.create count in
	let fd = Ssl.file_descr_of_socket socket in
	let bytes_read = ref 0 in
	let%lwt _ = while%lwt !bytes_read < count do
		let l, _, _ = Unix.select [fd] [] [] 0.1 in
		if List.length l > 0 then
			bytes_read := !bytes_read + Ssl.read socket message_bytes !bytes_read (count - !bytes_read);
		Lwt_main.yield ()
	done in
	return message_bytes

let rec read_all socket =
	Format.printf "@.";
	let%lwt bytes = read_blocking socket 8 in

	let bs = Bitstring.bitstring_of_string bytes in
	
	let _,
		(bep_version,
		(message_id,
		(message_type_int,
		(_, (* reserved field *)
		(compression,
		message_length))))) =
		Xdr.(read_bitstring xdr_message_header bs) in
	Printf.printf "bep version : %d\n" bep_version;
	Printf.printf "message_id : %d\n" message_id;
	Printf.printf "message_type : %d\n" message_type_int;
	Printf.printf "message_length : %d\n" message_length;
	Printf.printf "compression : %b\n" compression;

	let%lwt message_bytes = read_blocking socket message_length in
	let message_bs = Bitstring.bitstring_of_string message_bytes in
	let%lwt _ = 
	begin
	print_int message_type_int;
	print_endline "";
	match Config.type_from_int message_type_int with
	| ClusterConfig ->
		begin
			let bs, data = match_cluster_config message_bs in
			let%lwt index = build_index_message () in
			let wrapped_message = build_message_wrapper (Bitstring.string_of_bitstring index) (id()) Index in
			send_wrapped_message socket wrapped_message
		end
	| Index | IndexUpdate ->
		let (folder, (files, _)) = match_index message_bs in
		let share = Devicecontext.get_share folder in
		Share.feed_from_outside share files (fun j -> do_job socket j)

	| Response ->
		let cb = Jobs.find_callback message_id in
		let _, (data, code) = Xdr.(read_bitstring xdr_data message_bs) in
		let _ = wakeup cb (data, code) in
		return ()
	| Request ->
		begin
		let _,
			(folder,
			(filename,
			(offset,
			(size,
			(hash, _))))) = Xdr.read_bitstring xdr_request message_bs in

		Format.printf "answer request of %s %d %d@." filename (Int64.to_int offset) size;

		(* FIXME: check, insecure! *)
		let data_to_send =
			Share.share_get_block (Devicecontext.get_share folder) filename offset size in

		if Sha256.to_bin (Sha256.string data_to_send) <> hash then
			Format.printf "Hash is not the same than the one asked =(@.";

		let message = Xdr.to_string xdr_data (data_to_send, 0) in
		let wrapped_message = build_message_wrapper (Bitstring.string_of_bitstring message) (message_id) Response in
		send_wrapped_message socket wrapped_message
		end
	
	| Ping ->
		(* The reference implementation does not follow the spec, too bad *)
		(* let _ = Xdr.(read_bitstring xdr_ping message_bs) in *)
		let _ = send_bitstring socket (Xdr.to_string xdr_ping ("", 0)) in (print_endline "ping"; return ())
	| _ -> return @@ Printf.printf "Unknown message type.\n"
	end in
	return ()

let start_syncing () = (Devicecontext.load_shares () >>= init_ssl >>= connect_to "84.98.58.68" 12763 >>= send_greetings >>= fun socket ->
	(while%lwt true do
		read_all socket
	done;))
