open Core
open Parsing.Parse
open Typing.Type
open Typing.Pprint_typed_ast

let ( >>= ) r f = match r with Ok v -> f v | Error _ as e -> e

let compile ?(print_ast = false) (lexbuf : Lexing.lexbuf) =
  match parse_prog lexbuf >>= type_prog with
  | Ok prog ->
      if print_ast then printf "%s\n" (show_typed_program prog)
      else printf "Done!\n"
  | Error e -> eprintf "%s\n" (Error.to_string_hum e)

(* convert caml ast *)
