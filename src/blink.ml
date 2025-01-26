open Core
open Compiler

let get_file_ext filename =
  String.split_on_chars filename ~on:['.'] |> List.last |> Option.value ~default:""

let blink_file =
  let error_not_file filename =
    eprintf "'%s' is not a blink file" filename ;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match Sys_unix.file_exists filename with
      | `Yes           ->
          if get_file_ext filename |> String.equal "bl" then filename
          else error_not_file filename
      | `No | `Unknown -> error_not_file filename)

let command = 
  Command.basic ~summary:"Don't Blink."
  ~readme:(fun () -> "List of Blink options.")
  Command.Let_syntax.(
  let%map_open _print_ast = 
    flag "-print-init-ast" no_arg ~doc:"Pretty print the initial AST"
  and _print_tast = 
    flag "-print-typed-ast" no_arg ~doc:"Pretty print the typed AST"
  and filename = anon (maybe_with_default "-" ("filename" %: blink_file)) in 
  fun () -> 
    In_channel.with_file filename ~f:(fun ic -> 
      Lexing.from_channel ic |> compile ~_print_ast ~_print_tast
      ))


  let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
