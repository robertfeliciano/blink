open Core
open Lexer

let get_pos (pos : Lexing.position) =
  Fmt.str "%s: line %d, col %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

type diagnostic = { loc : Util.Range.t; message : string }

exception Unexpected_token of Lexing.position * Lexing.position

(* Keep shift/comparison tokens intact in expressions. In a type argument list,
   peel off one closing angle only when the original token cannot be accepted.
   The queue belongs to this parse, so separate files/compilations share no state. *)
let program lexbuf =
  let module I = Parser.MenhirInterpreter in
  let pending = ref [] in
  let last = ref (lexbuf.Lexing.lex_curr_p, lexbuf.lex_curr_p) in
  let next checkpoint =
    let token, start, stop =
      match !pending with
      | token :: rest ->
          pending := rest;
          token
      | [] ->
          let token = Lexer.read lexbuf in
          (token, lexbuf.lex_start_p, lexbuf.lex_curr_p)
    in
    let suffix =
      match token with
      | Parser.GTGT -> Some Parser.GT
      | Parser.GTGTGT -> Some Parser.GTGT
      | Parser.GTE -> Some Parser.EQUAL
      | Parser.GTGTEQ -> Some Parser.GTE
      | Parser.GTGTGTEQ -> Some Parser.GTGTEQ
      | _ -> None
    in
    let offered =
      match suffix with
      | Some suffix
        when (not (I.acceptable checkpoint token start))
             && I.acceptable checkpoint Parser.GT start ->
          let middle = { start with pos_cnum = start.pos_cnum + 1 } in
          pending := (suffix, middle, stop) :: !pending;
          (Parser.GT, start, middle)
      | _ -> (token, start, stop)
    in
    let _, start, stop = offered in
    last := (start, stop);
    offered
  in
  let rec loop checkpoint =
    match checkpoint with
    | I.InputNeeded _ -> loop (I.offer checkpoint (next checkpoint))
    | I.Shifting _ | I.AboutToReduce _ -> loop (I.resume checkpoint)
    | I.Accepted value -> value
    | I.HandlingError _ | I.Rejected ->
        let start, stop = !last in
        raise (Unexpected_token (start, stop))
  in
  loop (Parser.Incremental.program lexbuf.lex_curr_p)

let parse_prog_diagnostic (lexbuf : Lexing.lexbuf) =
  try Ok (program lexbuf) with
  | SyntaxError msg ->
      let err =
        Fmt.str "Syntax Error at %s: %s" (get_pos lexbuf.lex_curr_p) msg
      in
      Error { loc = Util.Range.lex_range lexbuf; message = err }
  | Unexpected_token (start, stop) ->
      let err = Fmt.str "Parser Error at %s" (get_pos stop) in
      Error { loc = Util.Range.mk_lex_range start stop; message = err }
  | Ast.InvalidObjectType loc ->
      Error
        {
          loc;
          message =
            Fmt.str
              "Parser Error at %s: object construction requires a class name \
               with explicit type arguments, not a type parameter."
              (Util.Range.string_of_range loc);
        }
  | Ast.ParserError (pos, msg) ->
      let err = Fmt.str "Parser Error at %s: %s" (get_pos pos) msg in
      Error { loc = Util.Range.mk_lex_range pos pos; message = err }

let parse_prog lexbuf =
  match parse_prog_diagnostic lexbuf with
  | Ok program -> Ok program
  | Error diagnostic -> Error (Error.of_string diagnostic.message)
