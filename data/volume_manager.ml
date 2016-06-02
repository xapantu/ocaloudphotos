open React
open Lwt

module type CONFIG = sig
end

module VolumeManager (Config:CONFIG) = struct

  module Devices = Devices.Devices
  module Objects = Objects.Object_manager

  exception Volume_not_found of string

  type volume = { id: string;
                  file_list: Shared.file list signal;
                  send_file_list: Shared.file list -> unit }

  let volume_list_files n =
    n.file_list

  let volumes = Hashtbl.create 10

  let new_volume_loaded, send_vol = E.create ()

  let all_volumes, send_all_volumes = S.create []

  let volume_from_id n =
    try
      Hashtbl.find volumes n
    with
    | Not_found -> raise (Volume_not_found n)


    let volume_id n = n.id

  let volume_path n = Sys.getcwd () ^ "/data2/" ^ volume_id n ^ "/"

  let public_volume n = true

  let scan_files v =
    let cwd = volume_path v in
    let rec add_files_from_directory path =
      let file_list = Sys.readdir (cwd ^ "/" ^ path) in
      Array.to_list file_list
      |> List.map (fun filename ->
        if Sys.is_directory (cwd ^ "/" ^ path ^ filename) then
          (Shared.{ name = path ^ filename; is_folder = true; }) :: add_files_from_directory (path ^ filename ^ "/")
        else
          [Shared.{name = path ^ filename; is_folder = false; }])
      |> List.concat
    in
    let filelist = add_files_from_directory "" in
    Lwt.return filelist

  let load_volume v =
    let%lwt file_list = scan_files v in
    let%lwt intfy = Lwt_inotify.create () in
    let%lwt watch = Lwt_inotify.add_watch intfy (volume_path v) [Inotify.(S_All)] in
    Lwt_main.yield () >>= (fun () ->
      while%lwt true do
        Lwt_main.yield () >>= (fun () ->
          let%lwt ev = Lwt_inotify.read intfy in
          let (_, kind_list, _, filename) = ev in
          if  List.mem Inotify.Create kind_list then
            let Some filename = filename in
            Lwt.return (v.send_file_list (Shared.{name = filename; is_folder = false; } :: S.value v.file_list))
          else if List.mem Inotify.Delete kind_list ||
                  List.mem Inotify.Moved_from kind_list then
            let%lwt file_list = scan_files v in
            Lwt.return (v.send_file_list file_list)
          else
            Lwt.return ())
      done;);
    Lwt.return (v.send_file_list file_list)

  let add_volume v =
    Hashtbl.add volumes v.id v;
    let%lwt () = load_volume v in
    let all_volumes = 
        Hashtbl.fold (fun _ a l -> a::l) volumes []
    in
    let () = send_all_volumes all_volumes
    in
    Lwt.return (send_vol v)

  let photos_volumes, send_photos_volumes = S.create []
  let files_volumes, send_files_volumes = S.create []

  let on_new_photo_volume, new_photo_volume = E.create ()
  let on_new_files_volume, new_files_volume = E.create ()

  let () =
    E.map (fun l ->
      send_photos_volumes (l:: S.value photos_volumes))
      on_new_photo_volume
    |> Lwt_react.E.keep
  and () =
    E.map (fun l ->
      send_files_volumes (l::S.value files_volumes))
      on_new_files_volume
    |> Lwt_react.E.keep

  let load_volumes () =
    let file_list, send_file_list = S.create [] in
    let c = {id = "california";
             file_list = file_list;
             send_file_list = send_file_list; }
    in add_volume c; new_photo_volume c;
    let file_list, send_file_list = S.create [] in
    {id = "low";
     file_list = file_list;
     send_file_list = send_file_list; }
    |> add_volume

  let volume_sync_for_device v device =
    let ev, send = E.create () in
    send 0.55;
    ev

  let volumes_enabled_for name =
    match name with
    | "photos" -> photos_volumes
    | "files" -> files_volumes

  let new_volume_enabled_for = function
    | "photos" -> on_new_photo_volume


end
