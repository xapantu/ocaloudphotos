
type _ atom =
  | TInt			: int atom
  | TBool    	  	: bool atom
  | TString   		: string atom
  | TInt64		 	: int64 atom

type (_) params_type =
  | TProd : ('a params_type * 'b params_type) -> ('a * 'b) params_type
  | TAtom : string * 'a atom -> 'a params_type

type 'a kvstore = 'a params_type * Sqlite3.db
 
let int (n : string) = TAtom (n, TInt)

let int64 n = TAtom (n, TInt64)

let bool (n : string) = TAtom (n, TBool)

let string (n : string) = TAtom (n, TString)

let ( ** ) t1 t2 = TProd (t1,t2)

exception Initialization_failure
exception Data_error of string * string
exception Update_error

let rec columns :type a. a params_type -> string list = function
	| TProd(a, b) -> columns a @ columns b
	| TAtom(n, _) -> [n]

let rec fst_param :type a. a params_type -> a -> string = fun p data ->
	match p with
	| TProd(a, b) ->
		let da, db = data in
		fst_param a da
	| TAtom(_, TString) -> data
	| TAtom(_) -> failwith "first column must be string"

let new_store :type a. a params_type -> string -> a kvstore = fun param filename ->
	let kv = Sqlite3.db_open filename in
	let cols = columns param in
	let cols = List.map (fun s -> s ^ " string") cols in
	let stmt = "create table if not exists store(" ^ (String.concat ", " cols) ^ ");" in
	match Sqlite3.exec kv stmt with
	| Sqlite3.Rc.OK -> param, kv
	| _ -> raise Initialization_failure

let all :type a. a kvstore -> a list = fun kv ->
	let param, sqlite = kv in
	let cols = columns param in
	let sql_query = ("select " ^ (String.concat ", " cols) ^ " from store") in
	let stmt = Sqlite3.prepare sqlite sql_query in 
	let all_data = ref [] in
	let pending_data = ref true in
	while !pending_data do
		match Sqlite3.step stmt with
		| Sqlite3.Rc.ROW ->
			let rec build_reply :type a. a params_type -> int -> int * a = fun param i ->
				match param with
				| TAtom(n, atom) ->
					let col_val = Sqlite3.column stmt i in
					begin
					match atom, col_val with
					| TInt, Sqlite3.Data.INT(v) -> i+1, Int64.to_int v
					| TInt64, Sqlite3.Data.INT(v) -> i+1, v
					| TString, Sqlite3.Data.TEXT(v) -> i+1, v
					| TString, Sqlite3.Data.INT(v) -> i+1, Int64.to_string v
					end
				| TProd(a, b) ->
					let i, v = build_reply a i in
					let i, vb = build_reply b i in
					i, (v, vb)
			in
			all_data := (snd @@ build_reply param 0)::!all_data
		| _ -> pending_data := false
	done;
	!all_data

let get :type a. a kvstore -> string -> a = fun kv key ->
	let param, sqlite = kv in
	let cols = columns param in
	let sql_query = ("select " ^ (String.concat ", " cols) ^ " from store where " ^ List.hd cols ^ " = ?") in
	let stmt = Sqlite3.prepare sqlite sql_query in 
	Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT(key));
	match Sqlite3.step stmt with
	| Sqlite3.Rc.ROW ->
		let rec build_reply :type a. a params_type -> int -> int * a = fun param i ->
			match param with
			| TAtom(n, atom) ->
				let col_val = Sqlite3.column stmt i in
				begin
				match atom, col_val with
				| TInt, Sqlite3.Data.INT(v) -> i+1, Int64.to_int v
				| TInt64, Sqlite3.Data.INT(v) -> i+1, v
				| TString, Sqlite3.Data.TEXT(v) -> i+1, v
				| TString, Sqlite3.Data.INT(v) -> i+1, Int64.to_string v
				end
			| TProd(a, b) ->
				let i, v = build_reply a i in
				let i, vb = build_reply b i in
				i, (v, vb)
		in
		snd @@ build_reply param 0
	| _ -> raise Not_found

let get_clued :type a. a kvstore -> a -> a = fun  kv full_data ->
	let param, sqlite = kv in
	let cols = columns param in
	let sql_query = ("select " ^ (String.concat ", " cols) ^ " from store where " ^ String.concat " and " (List.map (fun s -> s ^ " = ?") cols)) in
	let stmt = Sqlite3.prepare sqlite sql_query in
	let rec build_query :type a. a params_type -> a -> int -> int = fun param data i ->
		match param with
		| TAtom(n, atom) ->
			begin
			match atom with
			| TInt ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.INT(Int64.of_int data)) in
				i+1
			| TInt64 ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.INT(data)) in
				i+1
			| TString ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.TEXT(data)) in
				i+1
			end
		| TProd(a, b) ->
			let dataa, datab = data in
			let i = build_query a dataa i in
			build_query b datab i
	in
	build_query param full_data 1;
	match Sqlite3.step stmt with
	| Sqlite3.Rc.ROW -> full_data
	| _ -> get kv (fst_param param full_data)

let has_key kv key =
	try
		let _ = get kv key in true
	with | Not_found -> false

let add kv data =
	let param, sqlite = kv in
	let cols = columns param in
	let sql_query = "insert into store(" ^ (String.concat ", " cols) ^ ") values (" ^ String.concat ", " (List.map (fun s -> "?") cols) ^ ")" in
	let stmt = Sqlite3.prepare sqlite sql_query in 
	let rec build_query :type a. a params_type -> a -> int -> int = fun param data i ->
		match param with
		| TAtom(n, atom) ->
			begin
			match atom with
			| TInt ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.INT(Int64.of_int data)) in
				i+1
			| TInt64 ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.INT(data)) in
				i+1
			| TString ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.TEXT(data)) in
				i+1
			end
		| TProd(a, b) ->
			let dataa, datab = data in
			let i = build_query a dataa i in
			build_query b datab i
	in
	build_query param data 1;
	Sqlite3.step stmt

let del kv key =
	let param, sqlite = kv in
	let cols = columns param in
	let stmt = Sqlite3.prepare sqlite @@ "delete from store where" ^ List.hd cols ^ " = ?" in
	Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT(key));
	Sqlite3.step stmt

let del_data kv key data =
	let param, sqlite = kv in
	let cols = columns param in
	let stmt = Sqlite3.prepare sqlite @@ "delete from store where " ^ String.concat " and " (List.map (fun s -> s ^ " = ?") cols) in
	let rec build_query :type a. a params_type -> a -> int -> int = fun param data i ->
		match param with
		| TAtom(n, atom) ->
			begin
			match atom with
			| TInt ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.INT(Int64.of_int data)) in
				i+1
			| TInt64 ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.INT(data)) in
				i+1
			| TString ->
				let _ = Sqlite3.bind stmt i (Sqlite3.Data.TEXT(data)) in
				i+1
			end
		| TProd(a, b) ->
			let dataa, datab = data in
			let i = build_query a dataa i in
			build_query b datab i
	in
	build_query param data 1;
	Sqlite3.step stmt
