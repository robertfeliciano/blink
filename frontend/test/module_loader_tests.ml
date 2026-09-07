open OUnit2
open Ast
module Loader = Module_system.Module_loader
module Model = Module_system.Module_model

let parse source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "input.bl" };
  match Parsing.Parse.parse_prog_diagnostic lexbuf with
  | Ok program -> program
  | Error error -> assert_failure error.message

let import name =
  match parse ("import " ^ name ^ ";") with
  | Prog ([ import ], []) -> import
  | _ -> assert_failure "expected import"

let success = function
  | Ok value -> value
  | Error (error : Model.diagnostic) -> assert_failure error.message

let failure fragment = function
  | Ok _ -> assert_failure ("expected error: " ^ fragment)
  | Error (error : Model.diagnostic) ->
      assert_bool error.message (Core.String.is_substring error.message ~substring:fragment);
      error

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let roots context =
  let project = bracket_tmpdir context in
  let stdlib = bracket_tmpdir context in
  (project, stdlib, Model.{ project_root = project; stdlib_root = Some stdlib })

let test_lookup context =
  let project, stdlib, config = roots context in
  Unix.mkdir (Filename.concat project "app") 0o700;
  let file = Filename.concat project "app/geometry.bl" in
  write file "export class Circle {}";
  write (Filename.concat stdlib "io.bl") "export fun print() {}";
  assert_equal (Unix.realpath file) (success (Loader.resolve_path config (import "app.geometry")));
  assert_equal (Unix.realpath (Filename.concat stdlib "io.bl"))
    (success (Loader.resolve_path config (import "std.io")));
  let cwd = Sys.getcwd () in
  let other = bracket_tmpdir context in
  Fun.protect ~finally:(fun () -> Sys.chdir cwd) (fun () ->
    Sys.chdir other;
    assert_equal (Unix.realpath file) (success (Loader.resolve_path config (import "app.geometry"))));
  let missing = import "app.absent" in
  let error = failure "app/absent.bl" (Loader.resolve_path config missing) in
  assert_equal missing.loc error.loc;
  Unix.mkdir (Filename.concat project "directory.bl") 0o700;
  ignore (failure "not a regular file" (Loader.resolve_path config (import "directory")))

let test_std_isolation context =
  let project, _, config = roots context in
  Unix.mkdir (Filename.concat project "std") 0o700;
  write (Filename.concat project "std/io.bl") "";
  ignore (failure "io.bl" (Loader.resolve_path config (import "std.io")));
  ignore (failure "not configured"
    (Loader.resolve_path { config with stdlib_root = None } (import "std.io")));
  ignore (failure "after std" (Loader.resolve_path config (import "std")))

let test_symlinks context =
  let project, outside, config = roots context in
  let inside_file = Filename.concat project "actual.bl" in
  write inside_file "";
  Unix.symlink inside_file (Filename.concat project "alias.bl");
  assert_equal (Unix.realpath inside_file) (success (Loader.resolve_path config (import "alias")));
  (* A prefix-similar sibling directory must not pass the containment test. *)
  let sibling = project ^ "_outside" in
  Unix.mkdir sibling 0o700;
  Fun.protect ~finally:(fun () -> Sys.remove (Filename.concat sibling "item.bl"); Unix.rmdir sibling)
    (fun () ->
      let target = Filename.concat sibling "item.bl" in
      write target "";
      Unix.symlink target (Filename.concat project "escape.bl");
      ignore (failure "escapes" (Loader.resolve_path config (import "escape"))));
  Unix.symlink (Filename.concat outside "absent.bl") (Filename.concat project "broken.bl");
  ignore (failure "broken.bl" (Loader.resolve_path config (import "broken")))

let test_invalid_component context =
  let _, _, config = roots context in
  let original = import "safe" in
  List.iter (fun value ->
    let name = { original.elt.path.elt.name with elt = value } in
    let path = unqualified_name name in
    let bad = { original with elt = { original.elt with path } } in
    let error = failure "Invalid module" (Loader.resolve_path config bad) in
    assert_equal name.loc error.loc)
    [ ""; ".."; "a/b"; "/tmp"; "a\\b"; "1abc" ]

let test_bad_roots context =
  let project, _, config = roots context in
  let file = Filename.concat project "root-file" in
  write file "";
  ignore (failure "not a directory"
    (Loader.resolve_path { config with project_root = file } (import "item")));
  ignore (failure "absent"
    (Loader.resolve_path { config with project_root = Filename.concat project "absent" }
      (import "item")))

let test_permissions context =
  skip_if (Unix.geteuid () = 0) "root can read files regardless of permission bits";
  let project, _, config = roots context in
  let filename = Filename.concat project "private_file.bl" in
  write filename "";
  Unix.chmod filename 0;
  Fun.protect ~finally:(fun () -> Unix.chmod filename 0o600) (fun () ->
    ignore (failure "Permission denied" (Loader.resolve_path config (import "private_file")));
    ignore (failure "Permission denied" (Loader.parse_source ~id:[] ~filename)))

let test_parse_file context =
  let project, _, _ = roots context in
  let filename = Filename.concat project "helper.bl" in
  write filename "export fun answer() => i32 { return 42; }";
  let source = success (Loader.parse_source ~id:[ "helper" ] ~filename) in
  assert_equal filename source.filename;
  assert_equal [ "helper" ] source.id;
  (match source.program with
  | Prog ([], [ { elt = { export_loc = Some _; _ }; loc = (file, _, _) } ]) ->
      assert_equal filename file
  | _ -> assert_failure "export/source locations were lost");
  write filename "fun broken(\n";
  let error = failure filename (Loader.parse_source ~id:[ "helper" ] ~filename) in
  let file, _, _ = error.loc in
  assert_equal filename file;
  write filename "`";
  let lexical_error = failure "Syntax Error" (Loader.parse_source ~id:[] ~filename) in
  assert_equal (filename, (1, 1), (1, 2)) lexical_error.loc;
  (* A second read observes changed contents, without persistent caching. *)
  write filename "fun fixed() {}";
  ignore (success (Loader.parse_source ~id:[ "helper" ] ~filename));
  ignore (failure "not a regular file" (Loader.parse_source ~id:[] ~filename:project));
  ignore (failure "missing.bl" (Loader.parse_source ~id:[] ~filename:(Filename.concat project "missing.bl")))

let test_syntax _ =
  let program = parse "import std.io as io;\nexport inline fun f(x: geo.Circle) => geo.Circle { return x; }\nexport @C fun puts(s: string) => i32;\nexport class Box { let value: geo.Circle; }\nfun private_helper() {}" in
  (match program with
  | Prog ([ imp ], first :: rest) ->
      assert_equal "std.io" (show_qualified_name imp.elt.path);
      assert_equal (Some "io") (Option.map (fun n -> n.elt) imp.elt.alias);
      assert_equal (Some ("input.bl", (2, 1), (2, 7))) first.elt.export_loc;
      (match first.elt.declaration with
      | Function fn ->
          assert_bool "inline preserved" fn.elt.inline;
          (match fn.elt.args with
          | [ (TRef (RClass q), _) ] ->
              assert_equal "geo.Circle" (show_qualified_name q);
              assert_equal [ "geo"; "Circle" ] (List.map (fun n -> n.elt) (name_components q));
              assert_equal ("input.bl", (2, 24), (2, 27)) (List.hd q.elt.qualifiers).loc;
              assert_equal ("input.bl", (2, 28), (2, 34)) q.elt.name.loc
          | _ -> assert_failure "qualified type")
      | _ -> assert_failure "function");
      assert_equal [ true; true; false ] (List.map (fun d -> Option.is_some d.elt.export_loc) rest)
  | _ -> assert_failure "imports and declarations");
  List.iter (fun source ->
    match Parsing.Parse.parse_prog (Lexing.from_string source) with
    | Error _ -> ()
    | Ok _ -> assert_failure ("unexpectedly accepted: " ^ source))
    [ "import ;"; "import a.;"; "import a"; "import a as ;";
      "fun f() {} import a;"; "export import a;"; "export";
      "export export fun f() {}"; "export let x = 1;"; "export const x = 1;";
      "class C { export fun f() {} }"; "class C { export let x = 1; }";
      "fun f() { import a; }"; "fun f() { export let x = 1; }" ]

let test_qualified_uses _ =
  List.iter (fun source ->
    let program = parse source in
    match Typing.Type.type_prog program with
    | Ok _ -> assert_failure "unresolved class name reached typing"
    | Error error -> assert_bool (Core.Error.to_string_hum error)
        (Core.String.is_substring (Core.Error.to_string_hum error) ~substring:"Qualified class names"))
    [ "fun f(x: geo.Circle) {}";
      "fun f() { let x = new geo.Circle { radius = 1 }; }";
      "fun f(x: [geo.Circle; 2]) {}";
      "fun f(x: i32) { let y = x as geo.Circle; }";
      "fun f(callback: (geo.Circle) -> geo.Circle) {}" ];
  let program = parse "fun f(x: i32) { let y = (x as geo.Circle).radius; }" in
  match partition_declarations program with
  | [ fn ], [], [] ->
      (match fn.elt.body with
      | [ { elt = Decl (_, _, Some { elt = Proj ({ elt = Cast (_, TRef (RClass q)); _ }, "radius"); _ }, _); _ } ] ->
          assert_equal "geo.Circle" (show_qualified_name q)
      | _ -> assert_failure "parenthesized cast projection")
  | _ -> assert_failure "function"

let () = run_test_tt_main ("Module loading" >::: [
  "lookup" >:: test_lookup;
  "stdlib isolation" >:: test_std_isolation;
  "symlinks" >:: test_symlinks;
  "invalid components" >:: test_invalid_component;
  "bad roots" >:: test_bad_roots;
  "permissions" >:: test_permissions;
  "file parsing" >:: test_parse_file;
  "syntax" >:: test_syntax;
  "qualified uses" >:: test_qualified_uses;
])
