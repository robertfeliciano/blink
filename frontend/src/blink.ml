open Core
open Compiler
module Optimization_level = Util.Optimization_level

let get_file_ext filename =
  String.split_on_chars filename ~on:[ '.' ]
  |> List.last |> Option.value ~default:""

let blink_file =
  let error_not_file filename =
    eprintf "'%s' is not a blink file" filename;
    exit 1
  in
  Command.Spec.Arg_type.create (fun filename ->
      match Sys_unix.file_exists filename with
      | `Yes ->
          if get_file_ext filename |> String.equal "bl" then filename
          else error_not_file filename
      | `No | `Unknown -> error_not_file filename)

let select_optimization_level o0 o1 o2 o3 =
  match
    List.filter_opt
      [
        Option.some_if o0 Optimization_level.O0;
        Option.some_if o1 Optimization_level.O1;
        Option.some_if o2 Optimization_level.O2;
        Option.some_if o3 Optimization_level.O3;
      ]
  with
  | [] -> Optimization_level.default
  | [ level ] -> level
  | _ -> failwith "only one of -O0, -O1, -O2, or -O3 may be specified"

let command =
  Command.basic ~summary:"Don't Blink."
    ~readme:(fun () -> "List of Blink options.")
    Command.Let_syntax.(
      let%map_open print_ast =
        flag "-print-ast" no_arg ~doc:"Pretty print the initial AST"
      and print_tast =
        flag "-print-typed-ast" no_arg ~doc:"Pretty print the typed AST"
      and print_dast =
        flag "-print-desugared-ast" no_arg ~doc:"Pretty print the desugared AST"
      and o0 = flag "-O0" no_arg ~doc:"Use LLVM -O0 optimizations (default)"
      and o1 = flag "-O1" no_arg ~doc:"Use LLVM -O1 optimizations"
      and o2 = flag "-O2" no_arg ~doc:"Use LLVM -O2 optimizations"
      and o3 = flag "-O3" no_arg ~doc:"Use LLVM -O3 optimizations"
      and filename = anon (maybe_with_default "-" ("filename" %: blink_file)) in
      fun () ->
        let optimization_level = select_optimization_level o0 o1 o2 o3 in
        In_channel.with_file filename ~f:(fun ic ->
            let lexbuf = Lexing.from_channel ic in
            lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
            compile ~print_ast ~print_tast ~print_dast ~optimization_level
              lexbuf))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
