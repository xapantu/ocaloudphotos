open Lwt

type device = string
type shares = string

let get_device_from_id id = (id, ("ntu2", (1, ("", (0, ("", (Int64.zero, (0, []))))))))

let device_is_trusted dev = true

let read_cert_file infile = 
	let noeof = ref true in
	let s = ref "" in
	let f = open_in_bin infile in
	Sha256.to_bin @@ Sha256.channel f (-1)


let get_my_device () =
	get_device_from_id @@ read_cert_file "cert3.pem"

let my_device_shares:(string, Share.share) Hashtbl.t = Hashtbl.create 10

let get_shares () = Hashtbl.fold (fun a b c -> b::c) my_device_shares []

let load_shares () = 
	Format.printf "Loading shares…@.";
	let%lwt share = Share.load_from_disk (Share.init_share "mytest" (get_my_device ())) (fun () -> ()) in
	let _ = Hashtbl.add my_device_shares "mytest" share in
	(*let%lwt share = Share.load_from_disk (Share.init_share "default" (get_my_device ())) (fun () -> ()) in
	let _ = Hashtbl.add my_device_shares "default" share in*)
	return ()

let get_share name = Hashtbl.find my_device_shares name
