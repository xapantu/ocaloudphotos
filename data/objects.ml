open React

module type OBJ_MANAGER = sig
  (* first one is the concrete type, second one the internal one *)
  type _ object_type
  type _ object_data
  val save_object: 'a object_type -> 'a -> 'a object_data Lwt.t
  val link_to_parent: 'a object_data -> 'b object_data -> unit Lwt.t
  val get_object_of_type: 'a object_type -> 'a object_data list signal
  val object_is_outdated: 'a object_data -> bool
  val object_get_all_children: 'c object_data -> 'a object_type -> 'a object_data list signal
  val get: 'a object_type -> 'a object_data -> 'a
end

module Object_manager = struct

  type object_id = { timestamp: int; device: string; sha: string; id: string; }
  type 'a object_data = bytes * object_id

  type 'a object_type = string * (Protobuf.Decoder.t -> 'a) * ('a -> Protobuf.Encoder.t -> unit)

  exception Not_implemented

  let value_store = Ocsipersist.open_store "object-manager"

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

  let save_object (obj_type, dec, enc) data =
    let%lwt id = fresh_id () in
    let data_encoded = Protobuf.Encoder.encode_exn enc data in
    let table = Ocsipersist.open_table obj_type in
    let%lwt () = Ocsipersist.add table id.id (id, data_encoded) in
    Lwt.return (id, data_encoded)

  let link_to_parent (_, a) (_, b) =
    let table = Ocsipersist.open_table "_links" in
    Ocsipersist.add table (a.id ^ "|" ^ b.id) (a.id, b.id)

  let get_object_of_type (obj_type, dec, enc) =
    let table = Ocsipersist.open_table obj_type in
    let signal, signal_setter = S.create [] in
    let _ = 
      let%lwt all_objects = Ocsipersist.fold_step (fun n (id, data_encoded) q ->
      Lwt.return @@ Protobuf.Decoder.decode_exn dec data_encoded :: q) table [] in
      Lwt.return (signal_setter all_objects)
    in
    signal

    

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
