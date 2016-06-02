open React


module Object_manager = struct

  type object_id = { timestamp: int; device: string; sha: string; id: string; }
  type 'a object_data = bytes * object_id

  type 'a object_type = string * (Protobuf.Decoder.t -> 'a) * ('a -> Protobuf.Encoder.t -> unit)

  exception Not_implemented

  let value_store = Ocsipersist.open_store "object-manager"

  let all_signals = Hashtbl.create 50

  let fresh_id () =
    let%lwt myid = Ocsipersist.make_persistent value_store "ids" 1 in
    let%lwt current_id = Ocsipersist.get myid in
    let%lwt () = Ocsipersist.set myid (current_id + 1) in
    Lwt.return { timestamp = int_of_float @@ Unix.time ();
                 device = "mydevice";
                 sha = "mysha";
                 id = string_of_int current_id; }

  let get (_, dec, enc) (b, _) =
    Protobuf.Decoder.decode_exn dec b

  let get_signal name =
    try
      Lwt.return @@ fst @@ Hashtbl.find all_signals name
    with
    | Not_found ->
        let table = Ocsipersist.open_table name in
        let signal, signal_setter = S.create [] in
        let _ = 
          let%lwt all_objects = Ocsipersist.fold_step (fun n obj q ->
          Lwt.return (obj :: q)) table [] in
          Lwt.return (signal_setter all_objects)
        in
        (Hashtbl.add all_signals name (signal, signal_setter);
        Lwt.return signal)

  let get_signal_setter name =
    let%lwt _ = get_signal name in
    Lwt.return @@ snd @@ Hashtbl.find all_signals name

  let save_object (obj_type, dec, enc) data =
    let%lwt id = fresh_id () in
    let data_encoded = Protobuf.Encoder.encode_exn enc data in
    let table = Ocsipersist.open_table obj_type in
    let%lwt () = Ocsipersist.add table id.id (data_encoded, id) in
    let%lwt setter = get_signal_setter obj_type in
    let%lwt signal = get_signal obj_type in
    let () = setter ((data_encoded, id) :: (S.value signal)) in
    Lwt.return (data_encoded, id)

  let link_to_parent (_, a) (_, b) =
    let table = Ocsipersist.open_table "_links" in
    Ocsipersist.add table (a.id ^ "|" ^ b.id) (a.id, b.id)

  let get_object_of_type (obj_type, dec, enc) =
    get_signal obj_type

    
  let object_is_outdated _ =
    raise Not_implemented

  let object_get_all_children (_, id)  (obj_type, dec, enc) =
    let table = Ocsipersist.open_table "_links" in
    let obj_table = Ocsipersist.open_table obj_type in
    let signal, signal_setter = S.create [] in
    let _ =
      let%lwt all_objects = Ocsipersist.fold_step (fun n (aid, bid) q ->
        if aid = id then
          try
            let%lwt myobj = Ocsipersist.find obj_table bid in
            Lwt.return @@ myobj :: q
          with
          | Not_found -> Lwt.return q
        else Lwt.return q
      ) table []
      in
      Lwt.return (signal_setter all_objects)
    in
    signal

end
