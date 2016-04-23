open Eliom_lib
open Eliom_content
open Html5.D

open Markdown_ast

let to_html l =
	List.fold_left (fun l -> function
		| Paragraph(s) -> div [l; p [ pcdata s ]]
		| H1(s) -> div [l; h1[pcdata s]]
		) (pcdata "") l.blocks
	

let openfile path =
	let file_descr = Unix.openfile path [Unix.O_RDONLY] 0 in
	let input_channel = Unix.in_channel_of_descr file_descr in
	let file_data = Markdown_parser.file Markdown_lexer.read (Lexing.from_channel input_channel) in
	let file_data = { file_data with blocks = postprecess_md file_data.blocks; } in
	file_data
