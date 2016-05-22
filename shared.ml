type file = { name: string; is_folder: bool }

let file_parent_path f =
  try
    let last_slash = String.rindex f.name '/' in
    String.sub f.name 0 last_slash
  with
  | Not_found -> ""
