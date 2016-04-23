%token <string> STRING
%token <string>H1
%token NEW_LINE
%token EOF
(* part 1 *)
%start <Markdown_ast.file> file
%%
(* part 2 *)
file:
  | v = value; EOF { ({
 	options = [];
  	blocks = v}) }
  ;

(* part 3 *)
value:
  (*| s = STRING
    { [Markdown_ast.Paragraph s] }*)
  |  { [] }
  | suite = value; NEW_LINE;
    { suite }
  | suite = value; a = H1;
    { Markdown_ast.H1 a:: suite }
  | suite = value;s = STRING;
    { Markdown_ast.Paragraph s :: suite }
  ;
