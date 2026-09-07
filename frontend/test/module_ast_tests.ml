open OUnit2
open Ast

let parse source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "module.bl" };
  match Parsing.Parse.parse_prog lexbuf with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let declarations_source =
  "@C fun puts(text: string) => i32;\n\
   class Box { let value: i32 = 0; fun get() => i32 { return value; } }\n\
   fun main() => i32 { return 0; }"

let test_order_and_locations _ =
  let (Prog (imports, declarations)) = parse declarations_source in
  assert_equal [] imports;
  List.iteri
    (fun index top ->
      assert_equal None top.elt.export_loc;
      let filename, (line, _), _ = top.loc in
      assert_equal "module.bl" filename;
      assert_equal ~printer:string_of_int (index + 1) line)
    declarations;
  match declarations with
  | [ { elt = { declaration = Prototype pn; _ }; loc = ploc };
      { elt = { declaration = Class cn; _ }; loc = cloc };
      { elt = { declaration = Function fn; _ }; loc = floc } ] ->
      assert_equal ploc pn.loc;
      assert_equal cloc cn.loc;
      assert_equal floc fn.loc;
      assert_equal "puts" pn.elt.fname;
      assert_equal "Box" cn.elt.cname;
      assert_equal "main" fn.elt.fname
  | _ -> assert_failure "declaration source order was lost"

let test_partition_and_export _ =
  let original = parse declarations_source in
  let (Prog (imports, declarations)) = original in
  let export_loc = Util.Range.mk_range "module.bl" (1, 0) (1, 6) in
  let exported =
    List.map
      (fun top -> { top with elt = { top.elt with export_loc = Some export_loc } })
      declarations
  in
  let program = Prog (imports, exported) in
  assert_equal (partition_declarations original) (partition_declarations program);
  List.iter
    (fun top ->
      assert_equal (Some export_loc) top.elt.export_loc;
      assert_bool "printer must show explicit exports"
        (Core.String.is_prefix (show_top_level top) ~prefix:"export "))
    exported;
  match Typing.Type.type_prog program with
  | Error error -> assert_failure (Core.Error.to_string_hum error)
  | Ok _ -> ()

let located line first last value =
  { elt = value; loc = Util.Range.mk_range "module.bl" (line, first) (line, last) }

let test_located_import _ =
  let std = located 1 7 10 "std" in
  let io = located 1 11 13 "io" in
  let alias = located 1 17 24 "console" in
  let import : Module_system.Module_model.import =
    located 1 0 25 { path = located 1 7 13 { qualifiers = [ std ]; name = io }; alias = Some alias }
  in
  assert_equal [ std; io ] (name_components import.elt.path);
  assert_equal (Some alias) import.elt.alias;
  assert_equal "import std.io as console;" (show_import import);
  let unaliased = { import with elt = { import.elt with alias = None } } in
  assert_equal "import std.io;" (show_import unaliased);
  let (Prog (_, declarations)) = parse declarations_source in
  let program = Prog ([ import ], declarations) in
  assert_bool "program printer must include imports"
    (Core.String.is_substring (show_prog program) ~substring:"import std.io as console;");
  match Typing.Type.type_prog program with
  | Ok _ -> assert_failure "unresolved imports must not be silently ignored"
  | Error error ->
      let message = Core.Error.to_string_hum error in
      assert_bool "diagnostic must identify unresolved imports"
        (Core.String.is_substring message ~substring:"Imports must be resolved");
      assert_bool "diagnostic must identify the importing source"
        (Core.String.is_substring message ~substring:"module.bl")

let () =
  run_test_tt_main
    ("Module AST" >::: [
       "source order and locations" >:: test_order_and_locations;
       "exports and partition" >:: test_partition_and_export;
       "located imports" >:: test_located_import;
     ])
