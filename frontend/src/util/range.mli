type pos = int * int
type t = string * pos * pos

(* line of position *)
val line_of_pos : pos -> int

(* column of position *)
val col_of_pos : pos -> int

(* new position with given line and col *)
val mk_pos : int -> int -> pos

(* create a new range from the given filename and start, end positions *)
val mk_range : string -> pos -> pos -> t
val norange : t
val pos_of_lexpos : Lexing.position -> pos
val mk_lex_range : Lexing.position -> Lexing.position -> t
val lex_range : Lexing.lexbuf -> t
val string_of_range : t -> string
