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

let compile_program ?(print_ast = false) ?(print_tast = false)
    ?(print_dast = false)
    ?(optimization_level = Util.Optimization_level.default) program =
  Ok program
  >>= printer print_ast Ast.show_prog
  >>= type_prog ~optimization_level
  >>= printer print_tast show_typed_program
  >>= desugar_prog
  >>= printer print_dast show_desugared_program
  |> function
  | Ok p ->
      Out_channel.flush stdout;
      DT.convert_caml_ast p;
      Ok ()
  | Error _ as error -> error

(* Keep the existing lexbuf API. Imports without an origin/root are rejected by
   typing, rather than guessed relative to the process working directory. *)
let compile ?print_ast ?print_tast ?print_dast ?optimization_level lexbuf =
  parse_prog lexbuf
  >>= compile_program ?print_ast ?print_tast ?print_dast ?optimization_level
  |> function
  | Ok () -> ()
  | Error error -> eprintf "%s\n" (Error.to_string_hum error)

let compile_file ?module_root ?stdlib_root ?(print_ast = false) ?print_tast
    ?print_dast ?optimization_level filename =
  let config : Modules.Module_model.config =
    {
      project_root =
        Option.value module_root ~default:(Filename.dirname filename);
      stdlib_root;
    }
  in
  let module_result = function
    | Ok value -> Ok value
    | Error (diagnostic : Modules.Module_model.diagnostic) ->
        Error
          (Error.of_string
             (Util.Range.string_of_range diagnostic.loc
             ^ ": " ^ diagnostic.message))
  in
  Modules.Module_loader.load config ~entry_filename:filename
  |> module_result
  >>= (fun graph ->
  (* Source AST output keeps original spellings and filenames, one tree per
       file in dependency order. Typed/desugared output shows internal names. *)
  if print_ast then
    List.iter graph.dependency_order ~f:(fun source ->
        printf "Module %s (%s)\n%s\n"
          (String.concat ~sep:"." source.id)
          source.filename
          (Ast.show_prog source.program));
  Modules.Module_resolver.resolve graph |> module_result)
  >>= compile_program ?print_tast ?print_dast ?optimization_level
  |> function
  | Ok () -> Ok ()
  | Error error ->
      Error
        (Error.of_string
           (Modules.Module_symbols.display_names (Error.to_string_hum error)))
