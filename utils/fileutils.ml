let make_dir f =
	try
		Unix.mkdir f 0o750
	with | Unix.Unix_error(Unix.EEXIST, _, _) -> ()

let ensure_path filename =
	let rec ensure_path_aux i =
		try
			let i = String.index_from filename i '/' in
			let _ = make_dir (String.sub filename 0 i) in
			ensure_path_aux (i+1)
		with 
		| Not_found | Invalid_argument(_) -> ()
	in
	ensure_path_aux 1

