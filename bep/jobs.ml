type job =
	| FileRetrieve of string * string * int64 * int * ((string * int) Lwt.u) (* folder * filename * block offset * block size * callback *)
	| CopyData of ((string * int) Lwt.u) (* callback, dummy arguments *)
	| EmptyJob

let callbacks: (int * (string * int) Lwt.u) list ref = ref []

let add_callback id a = callbacks := (id, a)::!callbacks

let find_callback id = List.assq id !callbacks 


