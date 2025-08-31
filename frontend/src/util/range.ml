open Lexing

type pos = int * int (* line number and column *)
type t = string * pos * pos (* start pos and end pos *)

let norange = ("norange", (0, 0), (0, 0))
let line_of_pos (l, _) = l
let col_of_pos (_, c) = c
let mk_pos line col = (line, col)
let mk_range f s e = (f, s, e)

(* Creates a Range.pos from the Lexing.position data *)
let pos_of_lexpos (p : position) : pos =
  mk_pos p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let mk_lex_range (p1 : position) (p2 : position) : t =
  mk_range p1.pos_fname (pos_of_lexpos p1) (pos_of_lexpos p2)

let lex_range lexbuf : t =
  mk_lex_range (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

let string_of_range (f, (sl, sc), (el, ec)) =
  Printf.sprintf "%s[%d.%d-%d.%d]" f sl sc el ec
