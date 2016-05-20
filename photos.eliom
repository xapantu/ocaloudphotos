[%%shared
	open Eliom_lib
	open Eliom_content
	open Html5.D
	open Lwt
]

open Markdown_lexer

module Photos(E:App_stub.ENVBASE) = struct

	type 'a image = {
		filename:string;
		name:'a Eliom_content.Html5.D.elt;
		alt:string;
	}

	type 'a album = {
		name: string;
		id: string;
		path: string;
		description: 'a Eliom_content.Html5.D.elt;
		volume: E.Data.volume;
	}

	let read_images_from_album album =
		return []

	exception Album_does_not_exist

	let string_startswith a b =
		let n = String.length b in
		String.length a >= n && String.sub a 0 n = b

	let string_endswith a b = 
		let n = String.length b in
		let na = String.length a in
		String.length a >= n && String.sub a (na-n) n = b

	let string_endswiths a lb =
		List.fold_left (||) false @@ List.map (string_endswith a) lb

	let contains s1 s2 =
		let re = Str.regexp_string s2
		in
			try let _ = (Str.search_forward re s1 0) in true
			with Not_found -> false

	let read_images_from_album album =
		let extensions = [".jpg";".JPG";".png";".PNG";".jpeg";".JPEG"] in
		let dir = Unix.opendir album.path in
		let all_photos = ref [] in
		let end_loop = ref false in
		while not !end_loop do
			try
				let filename = Unix.readdir dir in
				if string_endswiths filename extensions then
					let caption_path = album.path ^ "/.captions/" ^ filename ^ ".md" in
					let name =
						try
							Markdown.to_html @@ Markdown.openfile caption_path
						with | Unix.Unix_error(_) -> pcdata ""
					in
					all_photos := {
						filename = filename;
						name = name;
						alt = "";
					} :: !all_photos
			with | End_of_file -> end_loop := true
		done;
		return !all_photos

	let read_album_information album =
		try%lwt
			let volume = E.Data.from_id album in
			let path = E.Data.volume_path volume in
			let index_file_data = Markdown.openfile (path ^ "/" ^ "index.md") in

			return {
				name = album;
				id = album;
				path = path;
				description = Markdown.to_html index_file_data;
				volume = E.Data.from_id album
			}
		with
		| Unix.Unix_error(_) -> raise Album_does_not_exist

	let main_service =
	  Eliom_service.App.service ~path:["p"] ~get_params:Eliom_parameter.(suffix @@ string "album") ()

    let () = E.Config.App.register
		~service:main_service
		(fun (album_id) () ->
			try%lwt
				let%lwt album = read_album_information album_id in
				let%lwt images = read_images_from_album album in
				let images = List.sort (fun i j -> String.compare i.filename j.filename) images in
				let displayed_images = List.map (fun album_img ->
					let img_uri = make_uri ~service:(E.Files.service_for_volume album.volume) (E.Files.download_path album.volume album_img.filename) in

					let html_img = img ~src:img_uri ~alt:album_img.alt () in
					let html_descr = div [album_img.name] in
					div [Raw.a ~a:[a_href img_uri] [html_img]; html_descr] ~a:[a_class ["myimg"]]) images
				in
				Lwt.return
				(Eliom_tools.F.html
					 ~title:"ocaloudphotos"
					 ~css:[["css";"ocaloudphotos.css"]]
					 ~js: [["js";"ocaloudphoto_handwritten.js"]]
					 Html5.F.(body (
					 album.description ::
					 displayed_images
					 )))
			with
			| Album_does_not_exist -> return
				(Eliom_tools.F.html
					 ~title:"ocaloudphotos"
					 ~css:[["css";"ocaloudphotos.css"]]
					 Html5.F.(body [
					 p [pcdata "This album does not seem to exist. Please make sure the directory exists and has an index.md file."];
					 ]))
		)
	
	let register_service_for_album n =
	  Eliom_service.preapply ~service:main_service n
	  |> E.Mimes.register_public n

	let _ =
		E.Data.volumes_enabled_for "photos"
		|> List.map E.Data.volume_id
		|> List.map register_service_for_album
end
