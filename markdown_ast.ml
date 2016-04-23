type block =
	| H1 of string
	| Paragraph of string

type file = {
	options: (string*string) list;
	blocks: block list;
}


let postprecess_md l = 
	let l = List.rev l in
	let rec postprocess_aux = function
		| Paragraph s :: H1 _ :: q -> H1 s :: postprocess_aux q
		| Paragraph s :: q -> Paragraph s :: postprocess_aux q
		| H1 s :: q -> Paragraph s :: postprocess_aux q
		| [] -> []
	in
	postprocess_aux l

