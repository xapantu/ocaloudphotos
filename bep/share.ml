open Lwt

module StringSet = Set.Make(String)

type share = (StringSet.t ref) * string * string * (string * string) Kvstore.kvstore (* id * path * hash * (file -> modified) *)

let name (_, n, _, _) = n

let modified (_, _, _, m) = m

let path (_, _, n, _) = n

let intfy (i, _, _, _) = i


let hash_file_store = Kvstore.(new_store (string "hash" ** string "file" ** string "offset") "hash.sqlite")


let get_current_time () = Int64.of_float (Unix.time ())

let get_modified path = Lwt_unix.stat path >>= fun r -> return @@ Int64.of_float r.st_mtime

let block_size = 131072

let get_blocks path =
	let f = open_in path in
	let blocks = ref [] in
	while pos_in f < in_channel_length f do
		let s = String.create block_size in
		try
			let () = really_input f s 0 block_size in
			blocks := (block_size, Sha256.to_bin (Sha256.string s))::!blocks;
		with
			| End_of_file -> let count = pos_in f mod block_size in
			blocks := (count, Sha256.to_bin (Sha256.string (String.sub s 0 count)))::!blocks;
	done;
	return @@ List.rev !blocks

let get_file_from_path modified share_path path =
	try%lwt
		let%lwt blocks = get_blocks (share_path ^ path) in
		let%lwt modified = get_modified (share_path ^ path) in
		return (path, ((0, (false, (false, (true, (false, (false, (false, 420))))))), (modified, ([(Int64.of_string "718406477900438571", Int64.of_int 1)], (get_current_time (), blocks)))))
	with
	| Sys_error(_) ->
		return (path, ((0, (false, (false, (true, (false, (false, (true, 420))))))), (modified, ([], (get_current_time (), [])))))

let rec lwt_list_eval = function
	| [] -> return []
	| t::q ->
		let%lwt a = t in
		let%lwt dst = lwt_list_eval q in
		return @@ a::dst

	
let rec lwt_list_concat = function
	| [] -> return []
	| t::q ->
		let%lwt a = t in
		let%lwt dst = lwt_list_concat q in
		return @@ List.concat [a; dst]

let rec lwt_list_sum = function
	| [] -> return 0
	| t::q ->
		let%lwt a = t in
		let%lwt dst = lwt_list_sum q in
		return @@ a + dst


let files share =
	let cwd = Sys.getcwd () in
	let cwd = (cwd ^ "/" ^ (path share) ^ "/") in
	let all_files = Kvstore.all (modified share) in
	let all_fileinfos = List.map (fun (filename, modified) ->
		get_file_from_path (Int64.of_string modified) cwd filename) all_files
	in lwt_list_eval all_fileinfos

let get_block filename offset size =
	let f = open_in_gen [Open_binary; Open_rdonly] 0 filename in
	let s = Bytes.create size in
	seek_in f (Int64.to_int offset); 
	really_input f s 0 size;
	close_in f;
	Bytes.to_string s

let share_get_block share filename = get_block ((path share) ^ "/" ^ filename)
	

let rec new_download_job share wait_before_start_writing waiter_to_wakeup
	file_descr filename offset size hash =

	Format.printf "Getting block for %s, %d\n" filename (Int64.to_int offset);

	let data, wait_for_data = Lwt.wait () in
	try
		begin
		let _, (backup_file, backup_offset) = Kvstore.get_clued hash_file_store (hash, (filename, (Int64.to_string offset))) in
		let backup_offset = Int64.of_string backup_offset in
		if backup_offset = offset && backup_file = filename then
			let _ = Lwt.bind (Lwt.return ()) (fun () ->
				let%lwt () = wait_before_start_writing in
				seek_out file_descr (pos_out file_descr + size);
				Lwt.wakeup waiter_to_wakeup ();
				Lwt.return None
			) in
			Jobs.EmptyJob
		else
			let _ = Printf.printf "need to write something\n" in
			let backup_data = get_block ((name share) ^ "/" ^ backup_file) backup_offset size in
			if (Sha256.to_bin (Sha256.string backup_data)) <> hash then
				(
				Printf.printf "Hash not the same as advertised, this should NOT happen\n";
				raise Not_found
				)
			else
				let _ = Lwt.bind data (fun (_, _) ->
					let%lwt () = wait_before_start_writing in
					output_string file_descr backup_data;
					Format.printf "frome cache write for %s at %s@." filename (Int64.to_string offset);
					Kvstore.add hash_file_store (hash, (filename, Int64.to_string offset));
					Lwt.wakeup waiter_to_wakeup ();
					Lwt.return None
				) in
				Jobs.CopyData(wait_for_data)
		end
	with
	| Not_found ->
		begin
		let j = Jobs.FileRetrieve(name share, filename, offset, size, wait_for_data) in

		Lwt.bind data (fun (data, error_code) ->
			let local_h = Sha256.to_bin (Sha256.string data) in
			if local_h = hash then
				begin
				let%lwt () = wait_before_start_writing in
				Format.printf "size %d correct download" (String.length data);
				output_string file_descr data;
				Format.printf "write for %s at %d\n@." filename (Int64.to_int offset);
				Kvstore.add hash_file_store (hash, (filename, Int64.to_string offset));
				Lwt.wakeup waiter_to_wakeup ();
				Lwt.return None
				end
			else
				Lwt.return @@ Some (new_download_job share wait_before_start_writing waiter_to_wakeup
									file_descr filename offset size hash);
		);
		j
		end

let start_inotify share callback =
	let%lwt intfy = Lwt_inotify.create () in
	let%lwt watch = Lwt_inotify.add_watch intfy (path share) [Inotify.(S_All)] in
	while%lwt true do
		Lwt_main.yield () >>= (fun () ->
			let%lwt ev = Lwt_inotify.read intfy in
			let (_, kind_list, _, filename) = ev in
			(if  List.mem Inotify.Create kind_list ||
				List.mem Inotify.Modify kind_list ||
				List.mem Inotify.Moved_to kind_list then
				let Some filename = filename in
				Format.printf "change in %s: %s@." filename @@ Inotify.string_of_event ev
			else if List.mem Inotify.Delete kind_list ||
					List.mem Inotify.Moved_from kind_list then
				let Some filename = filename in
				Format.printf "delete %s@." filename);
			return ())
	done;
	return ()

let freeze_inotify share f = ()

let unfreeze_inotify share f = ()

let notify_file share f = ()

let scan_files share =
	let cwd = Sys.getcwd () in
	let cwd = (cwd ^ "/" ^ (path share)) in
	let rec add_files_from_directory path =
		Format.printf "initial scan - browsing %s@." path;
		let file_list = Array.to_list (Sys.readdir (cwd ^ "/" ^ path)) in
		lwt_list_sum @@ List.map (fun filename ->
			if Sys.is_directory (cwd ^ "/" ^ path ^ filename) then
				add_files_from_directory (path ^ filename ^ "/")
			else
				
				let%lwt modified_time = get_modified (cwd ^ "/" ^ path ^ filename) in
				let%lwt update_available =
				try%lwt
					let _, known_modified_time = Kvstore.get (modified share) (path ^ filename) in
					let _ = Format.printf "update %s %s@." (Int64.to_string modified_time) known_modified_time in
					if known_modified_time <> Int64.to_string modified_time then
						return true
					else return false
				with | Not_found -> return true
				in
				if update_available then
					let _ = Kvstore.add (modified share) ((path ^ filename), Int64.to_string modified_time) in
					return 1
				else return 0) file_list
	in
	let%lwt filelist = add_files_from_directory "" in
	return filelist

let file_known share filename =
	try
		let _ = Unix.stat (path share ^ "/" ^ filename) in
		true
	with
	| Unix.Unix_error (Unix.ENOENT, _, _) -> false

let file_modified share filename =
	let stats = Unix.stat (path share ^ "/" ^ filename) in
	Int64.of_float stats.st_mtime

let acquire_lock share filename = return ()

let release_lock share filename = return ()

let download_file share download_function (filename, (flags, (modified, (_, (_, blocks))))) =
	Format.printf "Checking out  %s@." filename;
	
	if List.length blocks > 0 then
		begin
		freeze_inotify share filename;
		let current_offset = ref (Int64.zero) in
		let waiter = ref (Lwt.return ()) in
		let full_filename = ((name share) ^ "/" ^ filename) in
		Utils.Fileutils.ensure_path full_filename;
		let f = open_out_gen [Open_creat; Open_wronly] 0o750 full_filename in
		List.iter (fun (size, hash) ->
			let offset = !current_offset in
			let last_waiter = !waiter in
			let next_waiter, cb_waiter = Lwt.wait () in
			waiter := next_waiter;
			current_offset := Int64.add !current_offset (Int64.of_int size);

			download_function (new_download_job share last_waiter cb_waiter f filename offset size hash)
			) blocks;
		Lwt.bind !waiter (fun () -> close_out f;
			Unix.truncate full_filename (Int64.to_int !current_offset);
			Unix.utimes full_filename 0.0 @@ Int64.to_float modified;
			unfreeze_inotify share filename;
			notify_file share filename;
			Lwt.return ());
		end
	else return ()

let remove_from_share share filename =
	Lwt_unix.unlink ((path share) ^ "/" ^ filename)


let feed_from_outside share files download_function =
	Format.printf "Feeding %s from outside with %d files@." (name share) (List.length files);
	let _ = List.map (fun file ->
		let (filename, ((_, (_, (_, (_, (is_folder, (invalid, (deleted, mode))))))), (modified, (counters, (version, (blocks)))))) = file in 
		Format.printf "%s %b %d %d@." filename invalid mode @@ List.length blocks;
		if is_folder then return ()
		else
		begin
		let%lwt _ = acquire_lock share filename in
		let%lwt () =
			if (file_known share filename && modified > file_modified share filename) || not (file_known share filename) then
				begin
				if deleted && file_known share filename then
					remove_from_share share filename
				else if not deleted then
					download_file share download_function file
				else return ()
				end
			else
			return () in
		let%lwt _ = release_lock share filename in
		return ()
		end
	) files in return ()

let load_from_disk share file_changed_callback =
	Format.printf "Loading share %s from disk, " @@ name share;
	let _ = start_inotify share file_changed_callback in
	let%lwt updates_last_running = scan_files share in
	let _ = Format.printf "%d update(s) since last running.@." updates_last_running in
	return share

let init_share name device =
	(ref StringSet.empty, name, name, Kvstore.(new_store (string "hash" ** string "modified") (name ^ "_hash.sqlite")))

let to_xdr device (_, name, _, _)  =
	(name, [device]), (0, [])
	
