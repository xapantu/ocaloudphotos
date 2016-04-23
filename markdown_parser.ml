
exception Error

let _eRR =
  Error

type token = 
  | STRING of (
# 1 "markdown_parser.mly"
       (string)
# 12 "markdown_parser.ml"
)
  | NEW_LINE
  | H1 of (
# 2 "markdown_parser.mly"
       (string)
# 18 "markdown_parser.ml"
)
  | EOF

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state

let rec _menhir_goto_value : _menhir_env -> 'ttv_tail -> 'tv_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27 * 'tv_value) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, v) = _menhir_stack in
        let _2 = () in
        let _v : (
# 6 "markdown_parser.mly"
       (Markdown_ast.file)
# 49 "markdown_parser.ml"
        ) = 
# 10 "markdown_parser.mly"
                   ( ({
 	options = [];
  	blocks = v}) )
# 55 "markdown_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = _menhir_stack in
        let (_v : (
# 6 "markdown_parser.mly"
       (Markdown_ast.file)
# 62 "markdown_parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "markdown_parser.mly"
       (Markdown_ast.file)
# 69 "markdown_parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        let (_1 : (
# 6 "markdown_parser.mly"
       (Markdown_ast.file)
# 76 "markdown_parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv6)) : 'freshtv8)) : 'freshtv10)) : 'freshtv12)) : 'freshtv14)
    | H1 _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * 'tv_value) = Obj.magic _menhir_stack in
        let (_v : (
# 2 "markdown_parser.mly"
       (string)
# 85 "markdown_parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * 'tv_value) = Obj.magic _menhir_stack in
        let (a : (
# 2 "markdown_parser.mly"
       (string)
# 93 "markdown_parser.ml"
        )) = _v in
        ((let (_menhir_stack, suite) = _menhir_stack in
        let _v : 'tv_value = 
# 23 "markdown_parser.mly"
    ( Markdown_ast.H1 a:: suite )
# 99 "markdown_parser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _v) : 'freshtv16)) : 'freshtv18)
    | NEW_LINE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * 'tv_value) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, suite) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_value = 
# 21 "markdown_parser.mly"
    ( suite )
# 113 "markdown_parser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _v) : 'freshtv20)) : 'freshtv22)
    | STRING _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * 'tv_value) = Obj.magic _menhir_stack in
        let (_v : (
# 1 "markdown_parser.mly"
       (string)
# 122 "markdown_parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * 'tv_value) = Obj.magic _menhir_stack in
        let (s : (
# 1 "markdown_parser.mly"
       (string)
# 130 "markdown_parser.ml"
        )) = _v in
        ((let (_menhir_stack, suite) = _menhir_stack in
        let _v : 'tv_value = 
# 25 "markdown_parser.mly"
    ( Markdown_ast.Paragraph s :: suite )
# 136 "markdown_parser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _v) : 'freshtv24)) : 'freshtv26)) : 'freshtv28)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 6 "markdown_parser.mly"
       (Markdown_ast.file)
# 155 "markdown_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
    ((let _v : 'tv_value = 
# 19 "markdown_parser.mly"
     ( [] )
# 177 "markdown_parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _v) : 'freshtv2)) : 'freshtv4))

# 220 "/home/xapantu/.opam/4.02.2/lib/menhir/standard.mly"
  


# 185 "markdown_parser.ml"
