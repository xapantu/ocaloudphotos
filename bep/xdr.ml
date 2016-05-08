type +'a param_name = string

type _ atom =
  | TInt			: int atom
  | TBool    	  	: bool atom
  | TString   		: string atom
  | TFlag		 	: int -> int atom
  | TInt64		 	: int64 atom

type (_,_) params_type =
  | TProd : ( ('a, 'an) params_type * ('b, 'bn) params_type) -> (('a * 'b), 'an * 'bn) params_type
  | TList : ( ('a, 'an) params_type * string) -> ('a list, 'an * string) params_type
  | TAtom : (string * 'a atom) -> ('a, [`One of 'a] param_name) params_type
 
let int (n : string) = TAtom (n,TInt)

let flag (length:int) (n : string) = TAtom (n,TFlag(length))

let int64 n = TAtom (n, TInt64)

let bool (n : string) = TAtom (n,TBool)

let string (n : string) = TAtom (n,TString)

let list p n = TList(p, n)

let prod t1 t2 = TProd (t1,t2)

let ( ** ) = prod

let modp = fun a b ->
	if a mod b >= 0 then a mod b
	else (a mod b) + b


exception ProtocolError of string

let read_bitstring :type a c. (a, c) params_type -> Bitstring.bitstring -> Bitstring.bitstring * a = fun param bs ->
	let rec read_bitstring_aux : type a c. Bitstring.bitstring -> (a, c) params_type -> Bitstring.bitstring * a =  fun bs -> function
		| TAtom(n, TInt) ->
			(match%bitstring bs with
			| {| ent:32:int; _:-1:string, save_offset_to(offset) |} -> Bitstring.dropbits offset bs, Int32.to_int ent
			| {| _:-1:string |} -> raise (ProtocolError(n))
			)
		| TAtom(n, TFlag(i)) ->
			(match%bitstring bs with
			| {| ent:i:int; _:-1:string, save_offset_to(offset) |} -> Bitstring.dropbits offset bs, Int64.to_int ent
			| {| _:-1:string |} -> raise (ProtocolError(n))
			)
		| TAtom(n, TInt64) ->
			(match%bitstring bs with
			| {| ent:64:int; _:-1:string, save_offset_to(offset) |} -> Bitstring.dropbits offset bs, ent
			| {| _:-1:string |} -> raise (ProtocolError(n))
			)
		| TAtom(n, TBool) ->
			(match%bitstring bs with
			| {| ent:1:int |} -> Bitstring.dropbits 1 bs, ent
			| {| _:-1:string |} -> raise (ProtocolError(n))
			)
		| TProd(t1, t2) ->
			let bs1, a1 = read_bitstring_aux bs t1 in
			let bs2, a2 = read_bitstring_aux bs1 t2 in
			bs2, (a1, a2)
		| TList(t, n) ->
			(match%bitstring bs with
			| {| ent:32:int; _:-1:string, save_offset_to(offset) |} ->
				let bs = Bitstring.dropbits offset bs in
				let count = Int32.to_int ent in
				let rec build_ts bs = function
				| 0 -> bs, []
				| n -> let bs, a = read_bitstring_aux bs t in
					let bs, q = build_ts bs (n-1) in
					bs, a::q
				in
				build_ts bs count
			| {| _:-1:string |} -> raise (ProtocolError(n))
			)
		| TAtom(n, TString) ->
			(match%bitstring bs with
			| {| length:32:int;
				str: (Int32.to_int length)*8 : string;
				_ : (modp (- (Int32.to_int length)) 4)*8 : string;(* padding *)
				_:-1:string, save_offset_to(offset) |} -> Bitstring.dropbits offset bs, str
			| {| _:-1:string |} -> raise (ProtocolError(n))
			)
	in
	let bs, a = read_bitstring_aux bs param in bs,a

let to_string :type a c. (a, c) params_type -> a -> Bitstring.bitstring = fun param data ->
	let rec to_string_aux : type a c. (a, c) params_type -> a -> Bitstring.bitstring list = fun param data ->
		match param with
		| TAtom(n, TInt) ->
			let%bitstring b = {| Int32.of_int data:32 |} in [b]
		| TAtom(n, TFlag(i)) ->
			let%bitstring b = {| Int64.of_int data:i |} in [b]
		| TAtom(n, TInt64) ->
			let%bitstring b = {| data:64 |} in [b]
		| TAtom(n, TBool) ->
			let%bitstring b = {| data:1 |} in [b]
		| TProd(t1, t2) ->
			let d1, d2 = data in
			let l1 = to_string_aux t1 d1 in
			let l2 = to_string_aux t2 d2 in
			l1@l2
		| TList(t, n) ->
			let count = List.length data in
			let%bitstring b = {| Int32.of_int count:32 |}
			in b :: List.concat (List.map (to_string_aux t) data)
		| TAtom(n, TString) ->
			let str = Bitstring.bitstring_of_string data in
			let length = (Bitstring.bitstring_length str) / 8 in
			let%bitstring b = {| Int32.of_int length:32 |} in
			[b; str; Bitstring.create_bitstring @@ (modp (- length) 4)*8]
	in
	Bitstring.concat (to_string_aux param data)


