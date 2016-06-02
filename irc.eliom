[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
    open Lwt

type irc_message = {
  content:string; [@key 1]
  timestamp:float; [@key 2]
} [@@deriving protobuf]
]
let irc_message_type = "irc-message", irc_message_from_protobuf, irc_message_to_protobuf

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
    let%lwt _ = Env.Data.Objects.save_object irc_message_type {content="logged in"; timestamp=Unix.gettimeofday()} in
    let _ =
      sleep 20.0 >>= fun () ->Irc.send_join ~connection ~channel
    in
    Irc.listen ~connection ~callback:(
      fun connection result ->
        match result with
        | `Ok ({ command = PRIVMSG (_, msg) ; _ } as e)->
            let%lwt _ = Env.Data.Objects.save_object irc_message_type {content="priv " ^ to_string e; timestamp=Unix.gettimeofday()} in
          let msg = String.trim msg in
          let%lwt _ = Env.Data.Objects.save_object irc_message_type {content=msg; timestamp=Unix.gettimeofday()} in
          return ()
        | `Ok e ->
            let%lwt _ = Env.Data.Objects.save_object irc_message_type {content=to_string e; timestamp=Unix.gettimeofday()} in
          return ()
        | `Error e ->
            let%lwt _ = Env.Data.Objects.save_object irc_message_type {content=e; timestamp=Unix.gettimeofday()} in
          Lwt_io.printl e
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
         let irc_messages = irc_messages
                            |> React.S.map (List.map (Env.Data.Objects.get irc_message_type))
                            |> React.S.map (List.sort (fun i j -> compare j.timestamp i.timestamp))
                            |> Eliom_react.S.Down.of_react in
         let ul = 
           [%client
             ~%irc_messages
             |> React.S.map (List.map (fun l ->
               Html5.F.(li [pcdata l.content])
             ))
             |> React.S.map (fun l ->
               Html5.F.ul l)
             |> Html5.R.node
           ] |> Html5.C.node
         in
         Lwt.return (Env.F.main_box_sidebar [ul])
      )

end
