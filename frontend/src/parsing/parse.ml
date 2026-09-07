open Core
open Lexer

let get_pos (pos : Lexing.position) =
  Fmt.str "%s: line %d, col %d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

type diagnostic = { loc : Util.Range.t; message : string }

let parse_prog_diagnostic (lexbuf : Lexing.lexbuf) =
  try Ok (Parser.program Lexer.read lexbuf) with
  | SyntaxError msg ->
      let err =
        Fmt.str "Syntax Error at %s: %s" (get_pos lexbuf.lex_curr_p) msg
      in
      Error { loc = Util.Range.lex_range lexbuf; message = err }
  | Parser.Error ->
      let err = Fmt.str "Parser Error at %s" (get_pos lexbuf.lex_curr_p) in
      Error { loc = Util.Range.lex_range lexbuf; message = err }
  | Ast.ParserError (pos, msg) ->
      let err =
        Fmt.str "Parser Error at %s: %s" (get_pos pos) msg
      in
      Error { loc = Util.Range.mk_lex_range pos pos; message = err }

let parse_prog lexbuf =
  match parse_prog_diagnostic lexbuf with
  | Ok program -> Ok program
  | Error diagnostic -> Error (Error.of_string diagnostic.message)
