[@@@ocamlformat "disable"]

open Core
open Parsing.Parse
open Typing.Type
open Desugaring.Desugar
open Typing.Pprint_typed_ast
open Desugaring.Pprint_desugared_ast
module DT = Desugaring.Desugared_ast

let ( >>= ) r f = match r with Ok v -> f v | Error _ as e -> e

let printer should_print specific_printer prog = 
  if should_print then printf "%s\n" (specific_printer prog); 
  Ok prog

let compile ?(print_ast = false) ?(print_tast = false) ?(print_dast = false) (lexbuf : Lexing.lexbuf) =
  parse_prog lexbuf 
  >>= printer print_ast Ast.show_prog
  >>= type_prog
  >>= printer print_tast show_typed_program
  >>= desugar_prog
  >>= printer print_dast show_desugared_program
  |> function
  (* | Ok p -> DT.convert_caml_ast p *)
  | Ok _ -> printf "Done\n"
  | Error e -> eprintf "%s\n" (Error.to_string_hum e)
