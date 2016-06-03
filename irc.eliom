[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt

type irc_message = {
  content:string; [@key 1]
  timestamp:float; [@key 2]
  target:string; [@key 3]
} [@@deriving protobuf]

type irc_channel = {
  name:string; [@key 1]
  server:string; [@key 2]
} [@@deriving protobuf]
]
let irc_message_type = "irc-message", irc_message_from_protobuf, irc_message_to_protobuf
let irc_channel_type = "irc-channel", irc_channel_from_protobuf, irc_channel_to_protobuf

open React
  
open Lwt_unix
module Irc = Irc_client_lwt

module IrcApp(Env:App_stub.ENVBASE) = struct

  let main_service =
    Eliom_service.App.service ~path:["i"] ~get_params:Eliom_parameter.(unit) ()


  let server = "ulminfo.fr"
  let port = 3724
  let username = "ocaloud"
  let realname = "hihi"
  let nick = "ocaloud"
  let channel = "#ocaloud-dbg"

  let unwrap b =
    match b with
    | Some x -> x
    | None -> failwith "No value"

  let ping_server connection =
    let open Irc_message in
    let%lwt _ = Env.Data.Objects.save_object irc_message_type {content="logged in"; timestamp=Unix.gettimeofday(); target=server} in
    let _ =
      sleep 20.0 >>= fun () ->Irc.send_join ~connection ~channel
    in
    let%lwt all_channels = Env.Data.Objects.get_object_of_type irc_channel_type in
    let get_channel name =
      try
        List.find (fun p -> (Env.Data.Objects.get irc_channel_type p).name = name) (React.S.value all_channels)
        |> return
      with
      | Not_found -> Env.Data.Objects.save_object irc_channel_type {name; server;}
    in
    Irc.listen ~connection ~callback:(
      fun connection result ->
        match result with
        | `Ok ({ command = PRIVMSG (target, msg) ; _ } as e)->
          let msg = String.trim msg in
          let%lwt target_channel = get_channel target in
          let%lwt msg_obj = Env.Data.Objects.save_object irc_message_type {content=msg; timestamp=Unix.gettimeofday(); target} in
          let%lwt () = Env.Data.Objects.link_to_parent target_channel msg_obj in
          let%lwt msg_obj = Env.Data.Objects.save_object irc_message_type {content="priv " ^ to_string e; timestamp=Unix.gettimeofday();target=server} in
          let%lwt target_channel = get_channel server in
          let%lwt () = Env.Data.Objects.link_to_parent target_channel msg_obj in
          return ()
        | `Ok e ->
          let%lwt msg_obj = Env.Data.Objects.save_object irc_message_type {content=to_string e; timestamp=Unix.gettimeofday();target=server;} in
          let%lwt target_channel = get_channel server in
          let%lwt () = Env.Data.Objects.link_to_parent target_channel msg_obj in
          return ()
        | `Error e ->
          let%lwt msg_obj = Env.Data.Objects.save_object irc_message_type {content=e; timestamp=Unix.gettimeofday();target="error"} in
          let%lwt target_channel = get_channel server in
          let%lwt () = Env.Data.Objects.link_to_parent target_channel msg_obj in
          return ()
    )

  let a =
    Irc.connect_by_name ~server ~port ~username ~mode:0 ~realname ~nick ()
    >>= fun connection -> return (unwrap connection)
    >>= fun connection -> ping_server connection

  let () =
    Env.Config.App.register
      ~service:main_service
      (fun () () ->
         let%lwt irc_messages = Env.Data.Objects.get_object_of_type irc_message_type
         in
         let%lwt all_irc_channels = Env.Data.Objects.get_object_of_type irc_channel_type in
         let%lwt all_irc_channels  =
           all_irc_channels
           |> Lwt_react.S.map_s @@ Lwt_list.map_s (fun channel_object ->
             let%lwt channel_messages = Env.Data.Objects.object_get_all_children channel_object irc_message_type in
             Lwt_react.S.map (fun msgs ->
               channel_object, msgs) channel_messages
             |> return
           )
           >>= (fun s -> s
           |> React.S.map @@ React.S.merge (fun l s -> s :: l) []
           |> React.S.switch
           |> React.S.map @@ List.map (fun (l, c) -> Env.Data.Objects.get irc_channel_type l, (List.map (Env.Data.Objects.get irc_message_type)) c)
           |> React.S.map @@ List.map (fun (l, c) -> l, List.sort (fun i j -> compare i.timestamp j.timestamp) c)
           |> Eliom_react.S.Down.of_react
           |> return)
         in
         let all_messages =
           [%client
              ~%all_irc_channels
              |> React.S.map (List.map (fun (l, c) ->
                let all_messages = 
                  c
                  |> List.map (fun l ->
                    Html5.F.(li [pcdata l.content])
                  )
                  |> Html5.F.ul
                in
                Html5.F.(li [h1 [pcdata l.name]; all_messages])
              ))
              |> React.S.map (fun l ->
                Html5.F.ul l)
              |> Html5.R.node
           ] |> Html5.C.node
         in
         Lwt.return (Env.F.main_box_sidebar [all_messages])
      )

end
