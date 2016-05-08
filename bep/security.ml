open Utils

let get_id_from_certificate cert =
	let _ = Ssl.write_certificate (Config.config_dir ^ "/cert-tmp") cert in
	let sha_out = Unix.open_process_in @@ "openssl x509 -noout -in " ^ Config.config_dir ^ "/cert-tmp" ^"  -fingerprint -sha256" in
	let sha = input_line sha_out in
	let sha = String.sub sha 19 (String.length sha - 19) in
	let sha = (let s = ref "" in
		String.iter (fun c -> if c <> ':' then s := !s ^ (String.make 1 c);) sha; !s) in
	let sha = Strutils.hex_of_string_upper sha in
	B32.encode sha

