let string_startswith a b =
  let n = String.length b in
  String.length a >= n && String.sub a 0 n = b

let string_endswith a b = 
  let n = String.length b in
  let na = String.length a in
  String.length a >= n && String.sub a (na-n) n = b

let string_endswiths lb a =
  List.fold_left (||) false @@ List.map (string_endswith a) lb

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try let _ = (Str.search_forward re s1 0) in true
  with Not_found -> false

