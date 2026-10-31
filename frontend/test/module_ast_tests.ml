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
  | [
   { elt = { declaration = Prototype pn; _ }; loc = ploc };
   { elt = { declaration = Class cn; _ }; loc = cloc };
   { elt = { declaration = Function fn; _ }; loc = floc };
  ] ->
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
      (fun top ->
        { top with elt = { top.elt with export_loc = Some export_loc } })
      declarations
  in
  let program = Prog (imports, exported) in
  assert_equal
    (partition_declarations original)
    (partition_declarations program);
  List.iter
    (fun top ->
      assert_equal (Some export_loc) top.elt.export_loc;
      assert_bool "printer must show explicit exports"
        (Core.String.is_prefix (show_top_level top) ~prefix:"export "))
    exported;
  match Typing.Type.type_prog program with
  | Error error -> assert_failure (Core.Error.to_string_hum error)
  | Ok _ -> ()

let test_exported_global_location _ =
  let program = parse "export const answer: i32 = 42;\nlet counter: i32;" in
  match program with
  | Prog
      ( [],
        [
          {
            elt = { declaration = Global answer; export_loc = Some export_loc };
            _;
          };
          { elt = { declaration = Global counter; export_loc = None }; _ };
        ] ) ->
      assert_equal "answer" answer.elt.gname;
      assert_bool "export survives printing"
        (Core.String.is_substring (show_prog program) ~substring:"export ");
      assert_equal ("module.bl", (1, 1), (1, 7)) export_loc;
      let filename, (line, _), _ = answer.loc in
      assert_equal "module.bl" filename;
      assert_equal 1 line;
      (match answer.elt.ginit with
      | Some initial ->
          let file, (line, column), _ = initial.loc in
          assert_equal "module.bl" file;
          assert_equal 1 line;
          assert_equal 28 column
      | None -> assert_failure "missing constant initializer range");
      assert_equal None counter.elt.ginit
  | _ -> assert_failure "exported globals should preserve source ranges"

let located line first last value =
  {
    elt = value;
    loc = Util.Range.mk_range "module.bl" (line, first) (line, last);
  }

let test_located_import _ =
  let std = located 1 7 10 "std" in
  let io = located 1 11 13 "io" in
  let alias = located 1 17 24 "console" in
  let import : Modules.Module_model.import =
    located 1 0 25
      {
        path = located 1 7 13 { qualifiers = [ std ]; name = io };
        selection = Module_import (Some alias);
      }
  in
  assert_equal [ std; io ] (name_components import.elt.path);
  assert_equal (Module_import (Some alias)) import.elt.selection;
  assert_equal "import std.io as console;" (show_import import);
  let unaliased =
    { import with elt = { import.elt with selection = Module_import None } }
  in
  assert_equal "import std.io;" (show_import unaliased);
  let (Prog (_, declarations)) = parse declarations_source in
  let program = Prog ([ import ], declarations) in
  assert_bool "program printer must include imports"
    (Core.String.is_substring (show_prog program)
       ~substring:"import std.io as console;");
  match Typing.Type.type_prog program with
  | Ok _ -> assert_failure "unresolved imports must not be silently ignored"
  | Error error ->
      let message = Core.Error.to_string_hum error in
      assert_bool "diagnostic must identify unresolved imports"
        (Core.String.is_substring message ~substring:"Imports must be resolved");
      assert_bool "diagnostic must identify the importing source"
        (Core.String.is_substring message ~substring:"module.bl")

let test_named_imports _ =
  List.iter
    (fun source ->
      match parse source with
      | Prog
          ( [ ({ elt = { path; selection = Named_import names }; _ } as imp) ],
            [] ) ->
          assert_equal "app.helper" (show_qualified_name path);
          assert_equal [ "abc"; "xyz" ] (List.map (fun name -> name.elt) names);
          assert_equal ("module.bl", (1, 8), (1, 11)) (List.hd names).loc;
          assert_equal ("module.bl", (1, 13), (1, 16)) (List.nth names 1).loc;
          let printed = show_import imp in
          assert_equal "import abc, xyz from app.helper;" printed;
          assert_equal printed
            (match parse printed with
            | Prog ([ imp ], []) -> show_import imp
            | _ -> assert_failure "printed named import did not parse")
      | _ -> assert_failure "expected two located imported names")
    [ "import abc, xyz from app.helper;"; "import abc, xyz, from app.helper;" ];
  List.iter
    (fun source ->
      match parse source with
      | Prog ([ { elt = { selection = Named_import [ name ]; _ }; _ } ], []) ->
          assert_equal "abc" name.elt
      | _ -> assert_failure "expected one imported name")
    [ "import abc from helper;"; "import abc, from helper;" ]

let test_exported_generic _ =
  let source =
    parse "export fun identity<T>(value: T) => T { return value; }"
  in
  let functions, _, _, _ = partition_declarations source in
  assert_equal [ "T" ]
    (List.map (fun p -> p.pname.elt) (List.hd functions).elt.type_params);
  assert_bool "generic AST printer succeeds"
    (Core.String.is_substring (show_prog source) ~substring:"identity<T>")

let () =
  run_test_tt_main
    ("Module AST"
    >::: [
           "exported generic" >:: test_exported_generic;
           "source order and locations" >:: test_order_and_locations;
           "exports and partition" >:: test_partition_and_export;
           "exported global locations" >:: test_exported_global_location;
           "located imports" >:: test_located_import;
           "named imports and locations" >:: test_named_imports;
         ])
