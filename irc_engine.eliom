[%%shared
    open React
    open Lwt

    type irc_target = {
      channel:string; [@key 1]
      server:string; [@key 2]
    } [@@deriving protobuf]

    type irc_message = {
      content:string; [@key 1]
      timestamp:float; [@key 2]
      target:irc_target; [@key 3]
    } [@@deriving protobuf]

    type irc_channel = {
      name:string; [@key 1]
      server:string; [@key 2]
    } [@@deriving protobuf]

    type irc_account = {
      server: string; [@key 1]
      port: int; [@key 2]
      username: string; [@key 3]
      realname: string; [@key 4]
      nick: string; [@key 5]
    } [@@deriving protobuf]
]

let irc_message_type = "irc-message", irc_message_from_protobuf, irc_message_to_protobuf
let irc_channel_type = "irc-channel", irc_channel_from_protobuf, irc_channel_to_protobuf
let irc_account_type = "irc-account", irc_account_from_protobuf, irc_account_to_protobuf

module Irc = Irc_client_lwt

open Lwt_unix

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

module Irc_engine(Env:App_stub.ENVBASE) = struct

  let%lwt all_channels = Env.Data.Objects.get_object_of_type irc_channel_type

  let get_channel account name =
    try
      List.find (fun p -> (Env.Data.Objects.get irc_channel_type p).name = name) (React.S.value all_channels)
      |> return
    with
    | Not_found -> Env.Data.Objects.save_object irc_channel_type {name; server;}
  
  let new_message content target = 
    {content; target; timestamp = Unix.gettimeofday () }

  let dummy_account = {server; port; username; realname; nick; }
  
  let ping_server connection =
    let open Irc_message in
    let open Env.Data.Objects in
    let save_message = save_object irc_message_type in
    let%lwt _ = save_message (new_message ("Connected to " ^ server) {server; channel=server}) in
    let _ =
      sleep 20.0 >>= fun () -> Irc.send_join ~connection ~channel
    in
    Irc.listen ~connection ~callback:(
      fun connection result ->
        match result with
        | `Ok ({ command = PRIVMSG (channel, msg) ; _ } as e)->
          let msg = String.trim msg in
          let%lwt target_channel = get_channel dummy_account channel in
          let%lwt msg_obj = save_message (new_message msg { server; channel; }) in
          let%lwt () = link_to_parent target_channel msg_obj in
          let%lwt msg_obj = save_message (new_message (to_string e) { server; channel=server; }) in
          let%lwt target_channel = get_channel dummy_account server in
          let%lwt () = link_to_parent target_channel msg_obj in
          return ()
        | `Ok e ->
          let msg = new_message (to_string e) { server; channel=server; } in
          let%lwt msg_obj = save_message msg in
          let%lwt target_channel = get_channel dummy_account server in
          let%lwt () = link_to_parent target_channel msg_obj in
          return ()
        | `Error e ->
          let msg = new_message e { server; channel=server; } in
          let%lwt msg_obj = save_message msg in
          let%lwt target_channel = get_channel dummy_account server in
          let%lwt () = link_to_parent target_channel msg_obj in
          return ()
    )

  let _ =
    Irc.connect_by_name ~server ~port ~username ~mode:0 ~realname ~nick ()
    >>= fun connection -> return (unwrap connection)
    >>= fun connection -> ping_server connection

end
