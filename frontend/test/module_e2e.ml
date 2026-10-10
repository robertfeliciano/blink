open OUnit2
open Module_test_support
open Native_test_support

let compiler = executable_path "../src/blink.exe"
let wrapper = executable_path "../../../../compile"

let example_source name =
  Core.In_channel.read_all
    (executable_path ("../../../../examples/modules/" ^ name))

let stdlib_source = executable_path "../../../../runtime/stdlib/io.bl"

let command ?(options = []) filename =
  String.concat " "
    (List.map Filename.quote
       ((compiler :: "-module-root" :: Sys.getcwd () :: options) @ [ filename ]))

let verify_ir () =
  assert_success "LLVM verification"
    "llc --filetype=null new_output.ll -o /dev/null"

let native ?(options = []) name files =
  List.map
    (fun optimization ->
      name ^ optimization >:: fun context ->
      in_temp_dir ~prefix:"blink-module-native" context (fun () ->
          write_sources (Sys.getcwd ()) files;
          assert_success_silently "module compilation"
            (command ~options:(optimization :: options) "main.bl");
          verify_ir ();
          compile_and_run ~expected_exit:42))
    [ "-O0"; "-O2" ]

let negative ?(also = []) name files expected =
  name >:: fun context ->
  in_temp_dir ~prefix:"blink-module-errors" context (fun () ->
      write_sources (Sys.getcwd ()) files;
      assert_exit_code 1
        (command "main.bl" ^ " > compiler.stdout 2> compiler.stderr");
      let stderr = Core.In_channel.read_all "compiler.stderr" in
      List.iter
        (fun substring -> assert_contains ~substring stderr)
        (expected :: also);
      assert_bool "no output on failure" (not (Sys.file_exists "new_output.ll")))

let suite =
  "module native integration"
  >::: List.concat
         [
           native "global imported string identity"
             [
               ("state.bl", "export const message = \"shared\";");
               ( "main.bl",
                 {|import state;
const alias = state.message;
let copy = alias;
fun main() => i32 {
  if alias != state.message or copy != state.message { return 1; }
  return 42;
}|}
               );
             ];
           native "global diamond shared storage"
             [
               ( "state.bl",
                 "export let count = 40; export const increment = 1; export \
                  fun bump() => void { count += increment; }" );
               ( "left.bl",
                 "import state; export fun update() => void { state.bump(); }"
               );
               ( "right.bl",
                 "import state; export fun read() => i32 { return state.count; \
                  }" );
               ( "main.bl",
                 "import left; import right; import state as shared; fun \
                  main() => i32 { left.update(); shared.count += \
                  shared.increment; return right.read(); }" );
             ];
           native "global module isolation and constant imports"
             [
               ("left.bl", "export let value = 20; export const seed: i32 = 20;");
               ("right.bl", "export let value = 21;");
               ( "main.bl",
                 "import left; import right; const seed = left.seed; fun \
                  main() => i32 { left.value += 1; if right.value != 21 { \
                  return 1; } right.value = seed + 1; return left.value + \
                  right.value; }" );
             ];
           native "global imported strings and lambdas"
             [
               ( "state.bl",
                 "export let message = \"hello\"; export let count = 20; \
                  export fun bump() => void { count += 2; }" );
               ( "main.bl",
                 {|import state;
@C fun strcmp(left: string, right: string) => i32;
fun main() => i32 {
  let direct: () -> i32 = fn[]() { return state.count; };
  let snapshot: () -> i32 = fn[state.count]() { return state.count; };
  state.bump();
  if strcmp(state.message, "hello") != 0 { return 1; }
  state.message = "world";
  if strcmp(state.message, "world") != 0 { return 2; }
  let result = direct() + snapshot();
  free direct;
  free snapshot;
  return result;
}|}
               );
             ];
           [
             negative
               ~also:[ "main.bl:1:"; "first"; "second" ]
               "global cycle diagnostic"
               [
                 ( "main.bl",
                   "const first = second; const second = first; fun main() => \
                    i32 { return 0; }" );
               ]
               "Cyclic global initializer dependency";
             negative "global imported constant assignment"
               [
                 ("state.bl", "export const count = 42;");
                 ( "main.bl",
                   "import state; fun main() => i32 { state.count += 1; return \
                    state.count; }" );
               ]
               "const";
             negative "global imported private value"
               [
                 ("state.bl", "let secret = 42;");
                 ( "main.bl",
                   "import state; fun main() => i32 { return state.secret; }" );
               ]
               "private";
             negative "global mutable initializer through import"
               [
                 ("state.bl", "export let count = 42;");
                 ( "main.bl",
                   "import state; let copy = state.count; fun main() => i32 { \
                    return copy; }" );
               ]
               "mutable global";
           ];
           native
             ~options:[ "-stdlib-root"; Filename.dirname stdlib_source ]
             "module example"
             [
               ("main.bl", example_source "main.bl");
               ("helper.bl", example_source "helper.bl");
               ("geometry.bl", example_source "geometry.bl");
               ("state.bl", example_source "state.bl");
             ];
           native
             ~options:[ "-stdlib-root"; Filename.dirname stdlib_source ]
             "stdlib math"
             [
               ( "main.bl",
                 {|import std.math;

fun near(actual: f64, expected: f64) => bool {
    let difference = actual - expected;
    return difference > -0.000000000001 and difference < 0.000000000001;
}

fun main() => i32 {
    let quarter_pi: f64 = 0.7853981633974483;
    let half_pi: f64 = 1.5707963267948966;
    let two_pi: f64 = 6.283185307179586;
    let diagonal: f64 = 0.7071067811865476;

    if (not near(math.sin(0.0), 0.0) or not near(math.cos(0.0), 1.0)
        or not near(math.tan(0.0), 0.0)) { return 1; }
    if (not near(math.sin(quarter_pi), diagonal)
        or not near(math.cos(quarter_pi), diagonal)
        or not near(math.tan(quarter_pi), 1.0)) { return 2; }
    if (not near(math.sin(-quarter_pi), -diagonal)
        or not near(math.cos(-quarter_pi), diagonal)
        or not near(math.tan(-quarter_pi), -1.0)) { return 3; }
    if (not near(math.sin(half_pi), 1.0)
        or not near(math.cos(half_pi), 0.0)) { return 4; }
    if (not near(math.sin(two_pi + quarter_pi), diagonal)
        or not near(math.cos(two_pi + quarter_pi), diagonal)
        or not near(math.tan(two_pi + quarter_pi), 1.0)) { return 5; }

    if (not near(math.asin(diagonal), quarter_pi)
        or not near(math.acos(diagonal), quarter_pi)
        or not near(math.atan(1.0), quarter_pi)
        or not near(math.atan2(1.0, -1.0), 3.0 * quarter_pi)
        or not near(math.atan2(-1.0, 1.0), -quarter_pi)) { return 6; }
    if (not near(math.asin(-1.0), -half_pi)
        or not near(math.acos(-1.0), 2.0 * half_pi)
        or not near(math.atan(-1.0), -quarter_pi)) { return 7; }

    let sinh_one: f64 = 1.1752011936438014;
    let cosh_one: f64 = 1.5430806348152437;
    let tanh_one: f64 = 0.7615941559557649;
    if (not near(math.sinh(1.0), sinh_one)
        or not near(math.cosh(1.0), cosh_one)
        or not near(math.tanh(1.0), tanh_one)
        or not near(math.sinh(-1.0), -sinh_one)
        or not near(math.cosh(-1.0), cosh_one)
        or not near(math.tanh(-1.0), -tanh_one)) { return 8; }
    if (not near(math.asinh(sinh_one), 1.0)
        or not near(math.acosh(cosh_one), 1.0)
        or not near(math.atanh(tanh_one), 1.0)
        or not near(math.asinh(-sinh_one), -1.0)
        or not near(math.atanh(-tanh_one), -1.0)) { return 9; }
    if (not near(math.sinh(0.0), 0.0)
        or not near(math.cosh(0.0), 1.0)
        or not near(math.tanh(0.0), 0.0)
        or not near(math.asinh(0.0), 0.0)
        or not near(math.acosh(1.0), 0.0)
        or not near(math.atanh(0.0), 0.0)) { return 10; }

    if (not near(math.sqrt(9.0), 3.0)
        or not near(math.cbrt(27.0), 3.0)
        or not near(math.cbrt(-8.0), -2.0)
        or not near(math.hypot(3.0, 4.0), 5.0)
        or not near(math.hypot(-3.0, 4.0), 5.0)
        or not near(math.sqrt(0.0), 0.0)
        or not near(math.cbrt(0.0), 0.0)
        or not near(math.hypot(0.0, 0.0), 0.0)) { return 11; }

    if (not near(math.ln(1.0), 0.0)
        or not near(math.ln(2.718281828459045), 1.0)
        or not near(math.ln(0.5), -0.6931471805599453)
        or not near(math.log(8.0, 2.0), 3.0)
        or not near(math.log(1000.0, 10.0), 3.0)
        or not near(math.log(0.125, 2.0), -3.0)
        or not near(math.log(4.0, 0.5), -2.0)) { return 12; }
    return 42;
}
|}
               );
             ];
           native "nested aliases diamond private helpers"
             [
               ( "common.bl",
                 "fun seed() => i32 { return 21; } export fun answer() => i32 \
                  { return seed(); }" );
               ( "app/left.bl",
                 "import common; export fun read() => i32 { return \
                  common.answer(); }" );
               ( "app/right.bl",
                 "import common as shared; export fun read() => i32 { return \
                  shared.answer(); }" );
               ( "main.bl",
                 "import app.left as a; import app.right as b; fun main() => \
                  i32 { return a.read() + b.read(); }" );
             ];
           native "same spelling functions classes methods"
             [
               ( "left.bl",
                 "export class Box { let value: i32 = 20; fun read() => i32 { \
                  return value; } } export fun get() => Box { return new Box \
                  {}; }" );
               ( "right.bl",
                 "export class Box { let value: i32 = 22; fun read() => i32 { \
                  return this.value; } } export fun get() => Box { return new \
                  Box {}; }" );
               ( "main.bl",
                 "import left; import right; fun main() => i32 { let a: \
                  left.Box = left.get(); let b: right.Box = right.get(); let \
                  result = a.read() + b.read(); free a, b; return result; }" );
             ];
           native "constructors field defaults"
             [
               ( "geometry.bl",
                 "fun seed() => i32 { return 42; } export class Box { let \
                  value: i32 = seed(); fun Box() => Box { return new Box {}; } \
                  fun read() => i32 { return value; } }" );
               ( "main.bl",
                 "import geometry as shapes; fun main() => i32 { let b: \
                  shapes.Box; let result = b.read(); free b; return result; }"
               );
             ];
           native "module function values statement calls and closures"
             [
               ( "math.bl",
                 "export fun add(a: i32, b: i32) => i32 { return a + b; } \
                  export fun noop() => void {} export class Calc { let base: \
                  i32 = 10; fun add(a: i32, b: i32) => i32 { return base + a + \
                  b; } }" );
               ( "main.bl",
                 "import math as m; fun add() => i32 { return 100; } fun \
                  main() => i32 { m.noop(); let add = 2; let finish: (i32) -> \
                  i32 = fn[add](value) { return m.add(add, value); }; let \
                  apply: (i32) -> i32 = fn[finish](value) { return \
                  finish(value); }; let b = new m.Calc {}; let method: (i32) \
                  -> i32 = fn[b](value) { return b.add(20, value); }; let \
                  result = true ? apply(method(10)) : 0; free finish, apply, \
                  method, b; return result; }" );
             ];
           native "qualified types arrays casts typed lambdas"
             [
               ("geometry.bl", "export class Box { let value: i32 = 42; }");
               ( "main.bl",
                 "import geometry as g; fun main() => i32 { let box = new \
                  g.Box {}; let boxes: [g.Box; 1] = [box]; let read = fn[](b: \
                  g.Box) -> i32 { return (b as g.Box).value; }; let result = \
                  read(boxes[0]); free read; free box; return result; }" );
             ];
           native "captured module functions and returned closures"
             [
               ( "helper.bl",
                 "fun answer() => i32 { return 42; } export fun make() => () \
                  -> i32 { return fn[answer]() { return answer(); }; }" );
               ( "main.bl",
                 "import helper; fun main() => i32 { let f = helper.make(); \
                  let result = f(); free f; return result; }" );
             ];
           native "class fields in captured this lambda"
             [
               ( "helper.bl",
                 "export class Box { let value: i32 = 42; fun read() => i32 { \
                  let f: () -> i32 = fn[this]() { return value; }; let result \
                  = f(); free f; return result; } }" );
               ( "main.bl",
                 "import helper; fun main() => i32 { let b = new helper.Box \
                  {}; let result = b.read(); free b; return result; }" );
             ];
           native "C duplicates and Blink names do not collide"
             [
               ( "left.bl",
                 "@C fun abs(value: i32) => i32; export fun answer() => i32 { \
                  return abs(-20); }" );
               ( "right.bl",
                 "export @C fun abs(other: i32) => i32; export fun answer() => \
                  i32 { return abs(-20); }" );
               ( "main.bl",
                 "import left; import right; fun abs() => i32 { return 2; } \
                  fun main() => i32 { return left.answer() + right.abs(-20) + \
                  abs(); }" );
             ];
         ]
       @ [
           ( "exact C name and single main" >:: fun context ->
             in_temp_dir ~prefix:"blink-module-symbols" context (fun () ->
                 write_sources (Sys.getcwd ())
                   [
                     ("left.bl", "export @C fun abs(value: i32) => i32;");
                     ("right.bl", "export @C fun abs(other: i32) => i32;");
                     ( "main.bl",
                       "import left; import right; fun main() => i32 { return \
                        left.abs(-42); }" );
                   ];
                 assert_success_silently "compile symbols" (command "main.bl");
                 let ir = Core.In_channel.read_all "new_output.ll" in
                 assert_contains ~substring:"declare i32 @abs(i32)" ir;
                 assert_bool "no LLVM-renamed duplicate C prototype"
                   (not (contains ir "@abs."));
                 assert_contains ~substring:"define i32 @main(" ir;
                 verify_ir ();
                 compile_and_run ~expected_exit:42) );
           ( "stdlib output and isolation" >:: fun context ->
             in_temp_dir ~prefix:"blink-module-stdlib" context (fun () ->
                 write_sources (Sys.getcwd ())
                   [
                     ("library/io.bl", Core.In_channel.read_all stdlib_source);
                     ("std/io.bl", "this project file must never be parsed");
                     ( "main.bl",
                       "import std.io; fun main() => i32 { io.println(\"hello \
                        modules\"); return 42; }" );
                   ];
                 let library = Filename.concat (Sys.getcwd ()) "library" in
                 assert_success_silently "stdlib compile"
                   (command ~options:[ "-stdlib-root"; library ] "main.bl");
                 verify_ir ();
                 assert_success "lower stdlib"
                   "llc --filetype=obj --relocation-model=pic new_output.ll -o \
                    program.o";
                 assert_success "link stdlib" "clang program.o -o program -lm";
                 assert_exit_code 42 "timeout 10 ./program > program.stdout";
                 assert_equal "hello modules\n"
                   (Core.In_channel.read_all "program.stdout")) );
           ( "wrapper roots from another working directory" >:: fun context ->
             in_temp_dir ~prefix:"blink-module-wrapper" context (fun () ->
                 let root = Filename.concat (Sys.getcwd ()) "project" in
                 write_sources (Sys.getcwd ())
                   [ ("tools/compile", Core.In_channel.read_all wrapper) ];
                 let wrapper =
                   Filename.concat (Sys.getcwd ()) "tools/compile"
                 in
                 Unix.chmod wrapper 0o700;
                 Unix.symlink compiler
                   (Filename.concat (Sys.getcwd ()) "tools/blink");
                 write_sources root
                   [
                     ( "main.bl",
                       "import helper; fun main() => i32 { return \
                        helper.answer(); }" );
                     ("helper.bl", "export fun answer() => i32 { return 42; }");
                   ];
                 let invoke arguments =
                   String.concat " "
                     (List.map Filename.quote (wrapper :: arguments))
                 in
                 assert_success_silently "compile wrapper"
                   (invoke
                      [
                        "-module-root";
                        root;
                        Filename.concat root "main.bl";
                        "-O2";
                      ]);
                 assert_exit_code 42 "timeout 10 ./new_output.o";
                 (* Existing successful output must not be linked after a later failure. *)
                 write_sources root
                   [ ("helper.bl", "fun answer() => i32 { return 1; }") ];
                 assert_exit_code 1
                   (invoke
                      [ "-module-root"; root; Filename.concat root "main.bl" ]
                   ^ " > wrapper.stdout 2> wrapper.stderr");
                 assert_contains ~substring:"private"
                   (Core.In_channel.read_all "wrapper.stderr");
                 assert_bool "no successful build after error"
                   (not
                      (contains
                         (Core.In_channel.read_all "wrapper.stdout")
                         "Build completed"));
                 assert_exit_code 1
                   (invoke [ "-stdlib-root" ]
                  ^ " > wrapper.stdout 2> wrapper.stderr")) );
           ( "dependency edits and stable IR" >:: fun context ->
             in_temp_dir ~prefix:"blink-module-repeat" context (fun () ->
                 write_sources (Sys.getcwd ())
                   [
                     ("helper.bl", "export fun answer() => i32 { return 42; }");
                     ( "main.bl",
                       "import helper; fun main() => i32 { return \
                        helper.answer(); }" );
                   ];
                 assert_success_silently "first compile" (command "main.bl");
                 let first_ir = Core.In_channel.read_all "new_output.ll" in
                 assert_success_silently "same compile" (command "main.bl");
                 assert_equal first_ir
                   (Core.In_channel.read_all "new_output.ll");
                 write_sources (Sys.getcwd ())
                   [
                     ("helper.bl", "export fun answer() => i32 { return 43; }");
                   ];
                 assert_success_silently "edited dependency" (command "main.bl");
                 compile_and_run ~expected_exit:43) );
           "imported call arity"
           >::: List.concat_map
                  (fun (return_type, body) ->
                    List.concat_map
                      (fun (kind, declaration) ->
                        List.concat_map
                          (fun arguments ->
                            List.map
                              (fun context ->
                                negative
                                  (String.concat "-"
                                     [
                                       kind;
                                       return_type;
                                       (if arguments = "" then "zero" else "one");
                                       (if context = "" then "statement"
                                        else "expression");
                                     ])
                                  [
                                    ("helper.bl", declaration);
                                    ( "main.bl",
                                      "import helper; fun main() => i32 { "
                                      ^ context ^ "helper.target(" ^ arguments
                                      ^ "); return 0; }" );
                                  ]
                                  "invalid number of arguments")
                              [ ""; "let result = " ])
                          [ ""; "1" ])
                      [
                        ( "function",
                          "export fun target(left: i32, right: i32) => "
                          ^ return_type ^ " { " ^ body ^ " }" );
                        ( "prototype",
                          "export @C fun target(left: i32, right: i32) => "
                          ^ return_type ^ ";" );
                      ])
                  [ ("i32", "return left + right;"); ("void", "") ];
           negative "private import"
             [
               ("helper.bl", "fun secret() => i32 { return 42; }");
               ( "main.bl",
                 "import helper; fun main() => i32 { return helper.secret(); }"
               );
             ]
             "private";
           negative "imported type error retains source"
             [
               ("helper.bl", "export fun answer() => i32 { return true; }");
               ( "main.bl",
                 "import helper; fun main() => i32 { return helper.answer(); }"
               );
             ]
             "helper.bl";
           negative "imported syntax error retains source"
             [
               ("helper.bl", "export fun answer( broken syntax");
               ( "main.bl",
                 "import helper; fun main() => i32 { return helper.answer(); }"
               );
             ]
             "helper.bl";
           negative "cycle error"
             [
               ( "helper.bl",
                 "import main; export fun answer() => i32 { return 42; }" );
               ( "main.bl",
                 "import helper; fun main() => i32 { return helper.answer(); }"
               );
             ]
             "cycle";
           negative "missing file"
             [ ("main.bl", "import missing; fun main() => i32 { return 0; }") ]
             "missing.bl";
           negative "missing stdlib configuration"
             [ ("main.bl", "import std.io; fun main() => i32 { return 0; }") ]
             "Standard-library root is not configured";
           negative "C signature error"
             [
               ("left.bl", "@C fun abs(value: i32) => i32;");
               ("right.bl", "@C fun abs(value: string) => i32;");
               ( "main.bl",
                 "import left; import right; fun main() => i32 { return 0; }" );
             ]
             "Conflicting @C signatures";
           negative "readable nominal error"
             [
               ("left.bl", "export class Box { let value: i32 = 0; }");
               ("right.bl", "export class Box { let value: i32 = 0; }");
               ( "main.bl",
                 "import left; import right; fun main() => i32 { let box: \
                  left.Box = new right.Box {}; return 0; }" );
             ]
             "left.Box";
         ]

let () = run_test_tt_main suite
