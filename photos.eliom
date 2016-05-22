[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt
    type image = {
        filename:string;
        name: Markdown_ast.file;
        alt:string;
        download_path:string list;
    }
]

open Markdown_lexer
open React
  

module Photos(Env:App_stub.ENVBASE) = struct

  type album = {
    name: string;
    id: string;
    path: string;
    description: Widgets.div_content;
    volume: Env.Data.volume;
    image_list: image list React.signal;
  }

  let albums = Hashtbl.create 10

  let album_from_id = Hashtbl.find albums

  let add_album = Hashtbl.add albums

  
  exception Album_does_not_exist

  let string_startswith a b =
    let n = String.length b in
    String.length a >= n && String.sub a 0 n = b

  let string_endswith a b = 
    let n = String.length b in
    let na = String.length a in
    String.length a >= n && String.sub a (na-n) n = b

  let string_endswiths lb a =
    List.fold_left (||) false @@ List.map (string_endswith a) lb

  let contains s1 s2 =
    let re = Str.regexp_string s2
    in
    try let _ = (Str.search_forward re s1 0) in true
    with Not_found -> false

  let read_images_from_album album =
    let extensions = [".jpg";".JPG";".png";".PNG";".jpeg";".JPEG"] in
    let open React.S in
    let volume = Env.Data.volume_from_id album in
    let album_path = Env.Data.volume_path volume in
    let photo_files =
      Env.Data.volume_list_files volume
      |> map (List.filter (fun f ->
        string_endswiths extensions Shared.(f.name)))
      |> map (List.map @@ fun f->
          let caption_path = album_path ^ "/.captions/" ^ Shared.(f.name) ^ ".md" in
          let name =
            try
              Markdown.openfile caption_path
            with | Unix.Unix_error(_) -> Markdown.empty
          in
        { filename = album_path ^ "/" ^ f.name;
          name = name;
          alt = "";
          download_path = Env.Files.download_path volume f.name;
        }
      )
    in photo_files

  let read_album_information album =
    try
      let volume = Env.Data.volume_from_id album in
      let path = Env.Data.volume_path volume in
      let index_file_data = Markdown.openfile (path ^ "/" ^ "index.md") in

      let image_list = 
        read_images_from_album album (* all images of the album *)
        |> React.S.map (List.sort (fun i j -> String.compare i.filename j.filename)) (* sorted images *)
      in
      { name = album;
        id = album;
        path = path;
        description = Markdown.to_html index_file_data;
        volume = volume;
        image_list = image_list
      }
    with
    | Unix.Unix_error(_) -> raise Album_does_not_exist

  let main_service =
    Eliom_service.App.service ~path:["p"] ~get_params:Eliom_parameter.(suffix @@ string "album") ()

  let () = Env.Config.App.register
      ~service:main_service
      (fun (album_id) () ->
         try%lwt
           let album = album_from_id album_id in
           let files_service = Env.Files.service_for_volume album.volume in
           let images_list = Eliom_react.S.Down.of_react album.image_list in
           let image_grid_view =
             [%client
               let open Html5.F in
               ~%images_list (* every image to display *)
               |> React.S.map (List.map (fun album_img -> (* convert them to html elements *)
                 let img_uri = make_uri ~service:~%files_service album_img.download_path in

                 let html_img = img ~src:img_uri ~alt:album_img.alt () in
                 let html_descr = div [Markdown.to_html album_img.name] in
                 div [Raw.a ~a:[a_href img_uri] [html_img]; html_descr] ~a:[a_class ["full-page-photo"]]
               ))
               |> React.S.map div (* pack everyone in a div *)
               |> Html5.R.node (* transform the signal into a proper html node *)
             ]
             |> Html5.C.node
           in
           Lwt.return (Env.F.main_box_sidebar [album.description; image_grid_view])
         with
         | Album_does_not_exist -> return
                                     (Eliom_tools.F.html
                                        ~title:"ocaloudphotos"
                                        ~css:[["css";"ocaloudphotos.css"]]
                                        Html5.F.(body [
                                          p [pcdata "This album does not seem to exist. Please make sure the directory exists and has an index.md file."];
                                        ]))
      )

  let register_service_for_album album_id =
    let album = read_album_information album_id in
    let () = add_album album_id album in
    Eliom_service.preapply ~service:main_service album.id
    |> Env.Mimes.register_public album_id

  let () =
    Env.Data.new_volume_enabled_for "photos"
    |> E.map Env.Data.volume_id
    |> E.map register_service_for_album
    |> Lwt_react.E.keep

  let () =
    Env.Mimes.register_sidebar "photos" begin
      fun () ->
        let open Html5.F in
        
        let all_volumes =
          Env.Data.volumes_enabled_for "photos"
          |> S.map (List.map Env.Data.volume_id)
          |> Eliom_react.S.Down.of_react
        in

        let list_view:Widgets.div_content =
          [%client
            let open Html5.F in
            ~%all_volumes
            |> React.S.map begin
              fun volumes ->
                List.map (fun v ->
                  let service = ~%main_service in
                  let service:Common_client.unit_service =
                    Eliom_service.preapply ~service v
                  in
                  li [a ~service [pcdata v] ()]) volumes
                |> Widgets.F.list_view
            end
            |> Html5.R.node
          ]
          |> Html5.C.node
        in

        div [h1 [pcdata "photos"]; list_view]
    end

end
