open OUnit2

type fixture = { name : string; source : string; expected_exit : int }

let fixtures =
  [
    {
      name = "arithmetic";
      source = "fun main() => i32 { return 5 + 3 * 4; }";
      expected_exit = 17;
    };
    {
      name = "numeric-separators";
      source = "fun main() => i32 { return 1_000_000 / 10_000; }";
      expected_exit = 100;
    };
    {
      name = "function-call";
      source =
        "fun twice(value: i32) => i32 { return value * 2; }\n\
         fun main() => i32 { return twice(9) + 1; }";
      expected_exit = 19;
    };
    {
      name = "control-flow";
      source =
        "fun main() => i32 {\n\
        \  let value = 0;\n\
        \  while value < 5 { value += 1; }\n\
        \  if value == 5 { return 23; } else { return 24; }\n\
         }";
      expected_exit = 23;
    };
    {
      name = "conditional-assignment";
      source =
        "fun main() => i32 {\n\
        \  let value = 0;\n\
        \  value = true ? 40 : 1;\n\
        \  return value + 2;\n\
         }";
      expected_exit = 42;
    };
    {
      name = "nested-conditional";
      source =
        "fun main() => i32 {\n\
        \  let outer = false;\n\
        \  let inner = true;\n\
        \  return outer ? 1 : inner ? 42 : 2;\n\
         }";
      expected_exit = 42;
    };
    {
      name = "conditional-numeric-promotion";
      source =
        "fun main() => i32 {\n\
        \  let narrow: i16 = 10;\n\
        \  let wide: u16 = 42;\n\
        \  return false ? narrow : wide;\n\
         }";
      expected_exit = 42;
    };
    {
      name = "conditional-lazy-branch";
      source =
        "@C fun exit(status: i32) => void;\n\
         fun fail() => i32 { exit(99); return 0; }\n\
         fun main() => i32 { return true ? 42 : fail(); }";
      expected_exit = 42;
    };
    {
      name = "conditional-lambda";
      source =
        "fun main() => i32 {\n\
        \  let selected: (i32) -> i32 = true ? fn(value) { return value + 2; } \
         : fn(value) { return value + 3; };\n\
        \  let result = selected(40);\n\
        \  free selected;\n\
        \  return result;\n\
         }";
      expected_exit = 42;
    };
    {
      name = "conditional-anonymous-lambda-call";
      source =
        "fun main() => i32 { return (true ? fn[](value: i32) -> i32 { return \
         value + 2; } : fn[](value: i32) -> i32 { return value + 3; })(40); }";
      expected_exit = 42;
    };
    {
      name = "for-loop";
      source =
        "fun main() => i32 {\n\
        \  let total = 0;\n\
        \  for i in 0..5 { total += i; }\n\
        \  return total;\n\
         }";
      expected_exit = 10;
    };
    {
      name = "array-foreach";
      source =
        "fun main() => i32 {\n\
        \  let values = [1, 2, 3];\n\
        \  let total = 0;\n\
        \  for value in values { total += value; }\n\
        \  return total;\n\
         }";
      expected_exit = 6;
    };
    {
      name = "object-fields";
      source =
        "class Box {\n\
        \  let left: i32 = 0;\n\
        \  let right: i32 = 0;\n\
         }\n\
         fun main() => i32 {\n\
        \  let box = new Box { left = 20, right = 24 };\n\
        \  return box.left + box.right;\n\
         }";
      expected_exit = 44;
    };
    {
      name = "array-index";
      source =
        "fun main() => i32 {\n\
        \  let values = [4, 8, 15];\n\
        \  return values[2];\n\
         }";
      expected_exit = 15;
    };
    {
      name = "capturing-lambda";
      source =
        "fun main() => i32 {\n\
        \  let scale = 12;\n\
        \  let apply: (i32, i32) -> i32 = fn[scale](left, right) {\n\
        \    return left + right * scale;\n\
        \  };\n\
        \  let result = apply(3, 4);\n\
        \  free apply;\n\
        \  return result;\n\
         }";
      expected_exit = 51;
    };
    {
      name = "mixed-integer-promotion";
      source =
        "fun main() => i32 {\n\
        \  let signed: i16 = 10;\n\
        \  let unsigned: u16 = 20;\n\
        \  return signed + unsigned;\n\
         }";
      expected_exit = 30;
    };
    {
      name = "integer-float-promotion";
      source =
        "fun main() => i32 {\n\
        \  let integer: i32 = 2;\n\
        \  let decimal: f32 = 1.5;\n\
        \  let result = integer + decimal;\n\
        \  if result > 3.0 { return result as i32; } else { return 0; }\n\
         }";
      expected_exit = 3;
    };
    {
      name = "float-loop-default-step";
      source =
        "fun main() => i32 {\n\
        \  let count = 0;\n\
        \  for value in 0.0..3.0 { count += 1; }\n\
        \  return count;\n\
         }";
      expected_exit = 3;
    };
    {
      name = "full-void-calls-through-function-values";
      source =
        "class Box { let value: i32 = 0; } fun set(box: Box, value: i32) => \
         void { box.value = value; } fun main() => i32 { let box = new Box {}; \
         let setter = set; setter(box, 40); let add = fn[box](value: i32) -> \
         void { box.value += value; }; add(2); free add; let result = \
         box.value; free box; return result; }";
      expected_exit = 42;
    };
    {
      name = "returned-lambda-full-application";
      source =
        "fun make(offset: i32) => (i32, i32) -> i32 { return fn[offset](left, \
         right) { return offset + left + right; }; } fun main() => i32 { let \
         add = make(10); let result = add(20, 12); free add; return result; }";
      expected_exit = 42;
    };
    {
      name = "returned-lambda-immediate-full-application";
      source =
        "fun make() => (i32, i32) -> i32 { return fn(left, right) { return \
         left + right; }; } fun main() => i32 { return make()(20, 22); }";
      expected_exit = 42;
    };
    {
      name = "function-parameter-full-application";
      source =
        "fun apply(f: (i32) -> i32, value: i32) => i32 { return f(value); } \
         fun main() => i32 { let increment = fn[](value: i32) -> i32 { return \
         value + 1; }; let result = apply(increment, 41); free increment; \
         return result; }";
      expected_exit = 42;
    };
    {
      name = "full-call-arguments-evaluate-once-in-order";
      source =
        "class Counter { let value: i32 = 0; fun next() => i32 { value += 1; \
         return value; } } fun digits(a: i32, b: i32, c: i32) => i32 { return \
         a * 100 + b * 10 + c; } fun main() => i32 { let counter = new Counter \
         {}; let result = digits(counter.next(), counter.next(), \
         counter.next()); free counter; return result; }";
      expected_exit = 123;
    };
    {
      name = "prototype-function-value-full-call";
      source =
        "@C fun abs(value: i32) => i32; fun main() => i32 { let absolute = \
         abs; return absolute(-42); }";
      expected_exit = 42;
    };
    {
      name = "prototype-definition";
      source =
        "fun identity(value: i32) => i32;\n\
         fun identity(value: i32) => i32 { return value; }\n\
         fun main() => i32 { return identity(7); }";
      expected_exit = 7;
    };
    {
      name = "function-reassignment-through-if";
      source =
        "fun add(left: i32, right: i32) => i32 { return left + right; }\n\
         fun main() => i32 {\n\
        \  let f: (i32) -> i32 = fn(value) { return add(1, value); };\n\
        \  if true { f = fn(value) { return add(2, value); }; }\n\
        \  let result = f(40);\n\
        \  free f;\n\
        \  return result;\n\
         }";
      expected_exit = 42;
    };
    {
      name = "function-reassignment-through-loop";
      source =
        "fun add(left: i32, right: i32) => i32 { return left + right; }\n\
         fun main() => i32 {\n\
        \  let f: (i32) -> i32 = fn(value) { return add(1, value); };\n\
        \  let count = 0;\n\
        \  while count < 1 {\n\
        \    f = fn(value) { return add(2, value); };\n\
        \    count += 1;\n\
        \  }\n\
        \  let result = f(40);\n\
        \  free f;\n\
        \  return result;\n\
         }";
      expected_exit = 42;
    };
    {
      name = "function-reassignment-through-either-branch";
      source =
        "fun add(left: i32, right: i32) => i32 { return left + right; }\n\
         fun select(use_two: bool) => i32 {\n\
        \  let f: (i32) -> i32 = fn(value) { return add(0, value); };\n\
        \  if use_two { f = fn(value) { return add(2, value); }; } else { f = \
         fn(value) { return add(3, value); }; }\n\
        \  let result = f(40);\n\
        \  free f;\n\
        \  return result;\n\
         }\n\
         fun main() => i32 { return select(true) + select(false); }";
      expected_exit = 85;
    };
  ]

let stage_failure fixture stage error =
  assert_failure
    (Printf.sprintf "%s failed during %s: %s" fixture.name stage
       (Core.Error.to_string_hum error))

let check_frontend_stages fixture =
  let ast =
    match Parsing.Parse.parse_prog (Lexing.from_string fixture.source) with
    | Ok ast -> ast
    | Error error -> stage_failure fixture "parsing" error
  in
  let typed =
    match Typing.Type.type_prog ast with
    | Ok typed -> typed
    | Error error -> stage_failure fixture "type checking" error
  in
  match Desugaring.Desugar.desugar_prog typed with
  | Ok _ -> ()
  | Error error -> stage_failure fixture "desugaring" error

let test_fixture fixture test_context =
  check_frontend_stages fixture;
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir
    ~prefix:("blink-" ^ fixture.name ^ "-")
    test_context
    (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:fixture.source;
      let compile_command =
        Printf.sprintf "%s %s" (Filename.quote compiler)
          (Filename.quote "program.bl")
      in
      Native_test_support.assert_success_silently "Blink compiler"
        compile_command;
      Native_test_support.compile_and_run ~expected_exit:fixture.expected_exit)

let test_underapplication (name, source) test_context =
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir
    ~prefix:("blink-arity-" ^ name ^ "-")
    test_context
    (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:source;
      let command =
        Printf.sprintf "%s program.bl > compiler.stdout 2> compiler.stderr"
          (Filename.quote compiler)
      in
      Native_test_support.assert_exit_code 1 command;
      let diagnostic = Core.In_channel.read_all "compiler.stderr" in
      assert_bool
        ("expected an arity diagnostic, got: " ^ diagnostic)
        (Core.String.is_substring diagnostic
           ~substring:"invalid number of arguments");
      assert_bool "arity diagnostic identifies the source file"
        (Core.String.is_substring diagnostic ~substring:"program.bl");
      assert_bool "type errors must stop before LLVM emission"
        (not (Sys.file_exists "new_output.ll")))

let interface_fixtures =
  [
    {
      name = "interface-arguments-evaluated-left-to-right";
      source =
        {|interface I { fun apply(first: i32, callback: (i32) -> i32) => i32; }
class C impl I {
  fun apply(first: i32, callback: (i32) -> i32) => i32 { return first + callback(0); }
}
class Counter { let trace: i32 = 0; }
fun mark(counter: Counter, digit: i32) => i32 {
  counter.trace = counter.trace * 10 + digit; return digit;
}
fun make_callback(counter: Counter, digit: i32) => (i32) -> i32 {
  let amount = mark(counter, digit);
  return fn[amount](value) { return amount + value; };
}
fun main() => i32 {
  let c = new C {}; let item: I = c; let counter = new Counter {};
  let first = item.apply(mark(counter, 1), make_callback(counter, 2));
  let second = item.apply(mark(counter, 3), make_callback(counter, 4));
  item.apply(mark(counter, 5), make_callback(counter, 6));
  let trace = counter.trace;
  free c, counter;
  if first == 3 and second == 7 and trace == 123456 { return 42; }
  return 1;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-method-returned-closure";
      source =
        {|interface I { fun make(offset: i32) => (i32) -> i32; }
class C impl I {
  fun make(offset: i32) => (i32) -> i32 {
    return fn[offset](value) { return offset + value; };
  }
}
fun main() => i32 {
  let c = new C {}; let item: I = c;
  let add: (i32) -> i32 = item.make(2);
  let result = add(40); free add, c; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-runtime-choice";
      source =
        {|interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return 20; } }
class D impl I { fun value() => i32 { return 22; } }
fun read(x: I) => i32 { return x.value(); }
fun choose(c: C, d: D, useC: bool) => I {
  let x: I = c;
  if not useC { x = d; }
  return x;
}
fun main() => i32 {
  let c = new C {}; let d = new D {};
  let result = read(choose(c, d, true)) + read(choose(c, d, false));
  free c, d; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-void-dispatch-alias";
      source =
        {|interface I { fun add(amount: i32) => void; fun value() => i32; }
class C impl I {
  let n: i32 = 10;
  fun value() => i32 { return n; }
  fun add(amount: i32) => void { n += amount; }
}
fun mutate(x: I) => void { x.add(32); }
fun main() => i32 {
  let c = new C {}; let alias: I = c; mutate(alias);
  let result = c.n; free c; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-field-array-return";
      source =
        {|interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return 20; } }
class D impl I { fun value() => i32 { return 22; } }
class Holder { let item: I = null; }
fun make() => I { return new D {}; }
fun main() => i32 {
  let c = new C {}; let d: I = make();
  let h = new Holder { item = c };
  let items: [I; 2] = [h.item, d];
  let same: [I; 2] = items as [I; 2];
  let result = same[0].value() + same[1].value();
  free c, d, h; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-multiple-implementations";
      source =
        {|interface Left { fun value() => i32; }
interface Right { fun other() => i32; }
class C impl Left, Right {
  fun value() => i32 { return 20; }
  fun other() => i32 { return 22; }
}
fun left(x: Left) => i32 { return x.value(); }
fun right(x: Right) => i32 { return x.other(); }
fun main() => i32 {
  let c = new C {}; let result = left(c) + right(c); free c; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-receiver-evaluated-once";
      source =
        {|interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return 40; } }
class Counter { let calls: i32 = 0; }
fun get(c: C, counter: Counter) => I { counter.calls += 1; return c; }
fun main() => i32 {
  let c = new C {}; let counter = new Counter {};
  let result = get(c, counter).value() + counter.calls * 2;
  free c, counter; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-method-parameter-and-return";
      source =
        {|interface I { fun value() => i32; fun echo(other: I) => I; }
class C impl I {
  fun value() => i32 { return 20; }
  fun echo(other: I) => I { return other; }
}
class D impl I {
  fun value() => i32 { return 22; }
  fun echo(other: I) => I { return other; }
}
fun main() => i32 {
  let c = new C {}; let d = new D {}; let first: I = c;
  let second: I = first.echo(d);
  first.value();
  let result = first.value() + second.value();
  free c, d; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-null-field-and-default-local";
      source =
        {|interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return 42; } }
class Holder { let item: I; }
fun main() => i32 {
  let missing: I; let holder = new Holder { item = null };
  if missing != null { return 90; }
  if holder.item != null { return 91; }
  holder.item = new C {};
  let result = holder.item.value(); free holder.item, holder; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-conditional-aggregate";
      source =
        {|interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return 20; } }
class D impl I { fun value() => i32 { return 22; } }
fun choose(c: C, d: D, condition: bool) => I {
  let selected: I = condition ? c : d; return selected;
}
fun main() => i32 {
  let c = new C {}; let d = new D {};
  let result = choose(c, d, true).value() + choose(c, d, false).value();
  free c, d; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-concrete-class-method-ABI";
      source =
        {|class Box { let n: i32 = 42; }
interface I { fun echo(item: Box) => Box; }
class C impl I { fun echo(item: Box) => Box { return item; } }
fun main() => i32 {
  let c = new C {}; let item: I = c; let box = new Box {};
  let result = item.echo(box).n;
  free c, box; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-receiver-before-lambda-argument";
      source =
        {|interface I { fun apply(f: (i32) -> i32) => i32; }
class C impl I { fun apply(f: (i32) -> i32) => i32 { return f(20); } }
class Counter { let calls: i32 = 0; }
fun get(c: C, counter: Counter) => I {
  counter.calls = counter.calls * 10 + 1; return c;
}
fun mark(counter: Counter) => i32 {
  counter.calls = counter.calls * 10 + 2; return 10;
}
fun make_callback(counter: Counter) => (i32) -> i32 {
  let amount = mark(counter);
  return fn[amount](value) { return amount + value; };
}
fun main() => i32 {
  let c = new C {}; let counter = new Counter {};
  let result = get(c, counter).apply(make_callback(counter)) + counter.calls;
  free c, counter; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-conversion-from-concrete-array";
      source =
        {|interface I { fun value() => i32; }
class C impl I { let n: i32; fun value() => i32 { return n; } }
fun read(item: I) => i32 { return item.value(); }
fun main() => i32 {
  let c = new C { n = 20 }; let d = new C { n = 22 };
  let concrete: [C; 2] = [c, d];
  let result = read(concrete[0]) + read(concrete[1]);
  free c, d; return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-null-equality-and-free";
      source =
        {|interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return 42; } }
fun main() => i32 {
  let empty: I = null;
  let c = new C {}; let first: I = c; let second: I = c;
  if empty != null { return 90; }
  if first == null { return 91; }
  if first != second { return 92; }
  let result = first.value(); free first; return result;
}|};
      expected_exit = 42;
    };
  ]

let test_interface_fixture optimization fixture context =
  check_frontend_stages fixture;
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir
    ~prefix:("blink-" ^ fixture.name ^ "-")
    context
    (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:fixture.source;
      Native_test_support.assert_success_silently "interface compiler"
        (Printf.sprintf "%s %s program.bl" (Filename.quote compiler)
           optimization);
      Native_test_support.assert_success "LLVM verification"
        "llc --filetype=null new_output.ll -o /dev/null";
      Native_test_support.compile_and_run ~expected_exit:fixture.expected_exit)

let test_parse_failure_stops_at_parser _ =
  let source = "fun main( => i32 { return 1; }" in
  match Parsing.Parse.parse_prog (Lexing.from_string source) with
  | Error _ -> ()
  | Ok _ -> assert_failure "invalid syntax unexpectedly parsed"

let test_type_failure_stops_at_type_checker _ =
  let source = "fun main() => i32 { return true; }" in
  match Parsing.Parse.parse_prog (Lexing.from_string source) with
  | Error error ->
      assert_failure
        (Printf.sprintf "type-error fixture failed to parse: %s"
           (Core.Error.to_string_hum error))
  | Ok ast -> (
      match Typing.Type.type_prog ast with
      | Error _ -> ()
      | Ok _ -> assert_failure "invalid program unexpectedly type-checked")

let optimization_source =
  "fun main() => i32 {\n\
  \  let value = 0;\n\
  \  while value < 5 { value += 1; }\n\
  \  return value + 18;\n\
   }"

let test_optimization_levels test_context =
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir ~prefix:"blink-optimization-" test_context
    (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:optimization_source;
      let compile flag =
        let command =
          Printf.sprintf "%s %s %s" (Filename.quote compiler) flag
            (Filename.quote "program.bl")
        in
        Native_test_support.assert_success_silently
          (if Core.String.is_empty flag then "Blink compiler default" else flag)
          command;
        let ir = Core.In_channel.read_all "new_output.ll" in
        Native_test_support.compile_and_run ~expected_exit:23;
        ir
      in
      let default_ir = compile "" in
      let o0_ir = compile "-O0" in
      assert_equal ~msg:"the default must be exactly -O0" default_ir o0_ir;
      assert_bool "-O0 should preserve stack allocations"
        (Core.String.is_substring o0_ir ~substring:"alloca");
      Core.List.iter [ "-O1"; "-O2"; "-O3" ] ~f:(fun level ->
          let ir = compile level in
          assert_bool
            (Printf.sprintf "%s should run the standard optimizing pipeline"
               level)
            (not (Core.String.is_substring ir ~substring:"alloca"))))

let test_conflicting_optimization_levels test_context =
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir ~prefix:"blink-optimization-error-"
    test_context (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:optimization_source;
      let command =
        Printf.sprintf "%s -O1 -O2 %s > command.stdout 2> command.stderr"
          (Filename.quote compiler)
          (Filename.quote "program.bl")
      in
      match Core_unix.system command with
      | Error (`Exit_non_zero _) -> ()
      | status ->
          assert_failure
            (Printf.sprintf
               "conflicting optimization levels should fail, got: %s"
               (Core_unix.Exit_or_signal.to_string_hum status)))

let test_inline_function test_context =
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir ~prefix:"blink-inline-" test_context
    (fun () ->
      let source =
        "inline fun add_two(value: i32) => i32 {\n\
        \  let result = value + 2;\n\
        \  return result;\n\
         }\n\
         fun main() => i32 { return add_two(40); }"
      in
      Core.Out_channel.write_all "program.bl" ~data:source;
      let command =
        Printf.sprintf "%s -O0 %s" (Filename.quote compiler)
          (Filename.quote "program.bl")
      in
      Native_test_support.assert_success_silently "inline function compiler"
        command;
      let ir = Core.In_channel.read_all "new_output.ll" in
      assert_bool "the O0 always-inliner should remove the call"
        (not (Core.String.is_substring ir ~substring:"call i32 @add_two"));
      assert_bool "the fully inlined internal function should be removed"
        (not
           (Core.String.is_substring ir
              ~substring:"define internal i32 @add_two"));
      Native_test_support.compile_and_run ~expected_exit:42)

let suite =
  let executable_tests =
    List.map (fun fixture -> fixture.name >:: test_fixture fixture) fixtures
  in
  "Compiler end-to-end"
  >::: [
         "stage failures"
         >::: [
                "parsing" >:: test_parse_failure_stops_at_parser;
                "type checking" >:: test_type_failure_stops_at_type_checker;
              ];
         "optimization levels"
         >::: [
                "compile and execute" >:: test_optimization_levels;
                "reject conflicting flags"
                >:: test_conflicting_optimization_levels;
              ];
         "inline function" >:: test_inline_function;
         "calls require all arguments"
         >::: List.map
                (fun ((name, _) as fixture) ->
                  name >:: test_underapplication fixture)
                Call_arity_fixtures.underapplication;
         "interface runtime dispatch"
         >::: List.concat_map
                (fun fixture ->
                  List.map
                    (fun optimization ->
                      fixture.name ^ optimization
                      >:: test_interface_fixture optimization fixture)
                    [ "-O0"; "-O2" ])
                interface_fixtures;
         "native execution" >::: executable_tests;
       ]

let () = run_test_tt_main suite
