open OUnit2

type fixture = { name : string; source : string; expected_exit : int }

let fixtures =
  [
    {
      name = "globals-preserve-integer-bit-widths";
      source =
        {|const bits: u8 = 1;
let inverted = ~bits;
const negative: i8 = -1;
const step: i8 = 1;
let shifted = negative >> step;
fun main() => i32 {
  let runtime_bits = bits;
  if inverted != ~runtime_bits { return 1; }
  if shifted != (negative >> step) { return 2; }
  return 42;
}|};
      expected_exit = 42;
    };
    {
      name = "globals-preserve-string-reference-identity";
      source =
        {|const alias = original;
const original = "shared";
let copy = alias;
fun main() => i32 {
  let local = original;
  if alias != original or copy != local { return 1; }
  copy = "different";
  if alias != original { return 2; }
  return 42;
}|};
      expected_exit = 42;
    };
    {
      name = "globals-preserve-f32-rounding-on-widening";
      source =
        {|const base: f32 = 0.1;
let widened: f64 = base;
const integer_base: f32 = 16777216;
let integer_widened: f64 = integer_base;
fun main() => i32 {
  if widened != (base as f64) { return 1; }
  if integer_widened != (integer_base as f64) { return 2; }
  return 42;
}|};
      expected_exit = 42;
    };
    {
      name = "globals-cross-functions-and-shadowing";
      source =
        {|let count = 20;
fun add() => void { count += 2; }
fun local() => i32 { let count = 20; return count; }
fun main() => i32 { add(); return count + local(); }|};
      expected_exit = 42;
    };
    {
      name = "globals-lambda-access-and-capture";
      source =
        {|let count = 20;
fun main() => i32 {
  let direct: () -> i32 = fn[]() { return count; };
  let snapshot: () -> i32 = fn[count]() { return count; };
  count += 2;
  let result = direct() + snapshot();
  free direct;
  free snapshot;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "globals-class-defaults-and-methods";
      source =
        {|let count = 20;
class Box { let value: i32 = count; fun read() => i32 { return value + count; } }
fun main() => i32 {
  let box = new Box {};
  count = 22;
  let result = box.read();
  free box;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "globals-primitive-defaults-and-strings";
      source =
        {|@C fun strcmp(left: string, right: string) => i32;
let enabled: bool;
let ratio: f64;
let number: i32;
let greeting: string;
const expected = "world";
fun main() => i32 {
  if enabled { return 1; }
  if ratio != 0.0 { return 2; }
  if number != 0 { return 3; }
  if strcmp(greeting, "") != 0 { return 4; }
  greeting = expected;
  if strcmp(greeting, "world") != 0 { return 5; }
  return 42;
}|};
      expected_exit = 42;
    };
    {
      name = "globals-wide-and-narrow-constants";
      source =
        {|const signed: i128 = -170141183460469231731687303715884105728;
const unsigned: u128 = 340282366920938463463374607431768211455;
const half: f32 = -1.5;
const answer: i32 = future + 2;
const future: i8 = 40;
let unsigned_copy = unsigned;
fun main() => i32 {
  if signed != -170141183460469231731687303715884105728 { return 1; }
  if unsigned_copy != 340282366920938463463374607431768211455 { return 2; }
  if half != -1.5 { return 3; }
  return answer as i32;
}|};
      expected_exit = 42;
    };
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

let generic_fixtures =
  [
    {
      name = "generic-class-interface-and-function-inference";
      source =
        {|interface Value<T> { fun get() => T; }
class Box<T> impl Value<T> { let value: T; fun get() => T { return value; } }
class IntReader impl Value<i32> { fun get() => i32 { return 42; } }
class Node<T> { let value: T; let next: Node<T> = null; }
fun identity<T>(value: T) => T { return value; }
fun read<T>(box: Value<T>) => T { return box.get(); }
fun main() => i32 {
  let concrete = new IntReader {};
  if read(concrete) != 42 { return 2; }
  let box = new Box<i32> { value = 42 };
  let inferred = new Box { value = 42 };
  let node = new Node<i32> { value = 42 };
  node.next = node;
  let nested = new Box<Box<i32>> { value = box };
  let view: Value<i32> = identity(box);
  if read(box) != 42 or inferred.get() != 42 or node.next.value != 42 { return 1; }
  let result = read(view) + nested.get().get() - 42;
  free nested, node, inferred, box, concrete;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-required-reference-fields";
      source =
        {|let count = 0;
interface Value { fun get() => i32; }
class Number impl Value { let value: i32; fun get() => i32 { return value; } }
class Box<T> { let value: T; fun get() => T { return value; } }
fun identity<T>(value: T) => T { return value; }
fun main() => i32 {
  let number = new Number { value = 42 };
  let object = new Box<Number> { value = number };
  let view: Value = number;
  let contract = new Box<Value> { value = view };
  let direct = new Box<Value> { value = number as Value };
  let callback: (i32) -> i32 = identity;
  let function = new Box<(i32) -> i32> { value = callback };
  let lambda = new Box<(i32) -> i32> { value = fn(value) { return value + 2; } };
  let sink = new Box<() -> void> { value = fn[]() { count += 1; } };
  sink.value();
  let void_callback = sink.get();
  let inner = new Box<i32> { value = 42 };
  let nested = new Box<Box<i32>> { value = inner };
  let f = function.get();
  let g = lambda.get();
  if object.get().get() != 42 or contract.get().get() != 42 or direct.get().get() != 42 { return 1; }
  if f(42) != 42 or g(40) != 42 or nested.get().get() != 42 { return 2; }
  if function.value(42) != 42 or lambda.value(40) != 42 or count != 1 { return 3; }
  let result = inner.get();
  free object, contract, direct, function, lambda, sink, nested, inner;
  free callback, g, void_callback, number;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-constructor-field-evaluation-order";
      source =
        {|let trace = 0;
let count = 0;
fun mark(digit: i32) => i32 { trace = trace * 10 + digit; count += 1; return digit; }
class Record<T> { let first: i32; let callback: T; let second: i32; }
fun main() => i32 {
  let record = new Record<() -> i32> {
    second = mark(1),
    callback = mark(2) == 2 ? fn[]() { return 42; } : fn[]() { return 0; },
    first = mark(3)
  };
  let callback = record.callback;
  if trace != 123 or count != 3 or record.first != 3 or record.second != 1 { return 1; }
  let result = callback();
  free record, callback;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "dynamic-buffers-with-reference-payloads";
      source =
        {|interface Value { fun get() => i32; }
class Box impl Value { let value: i32; fun get() => i32 { return value; } }
fun identity<T>(value: T) => T { return value; }
fun allocate<T>(value: T) => [T] {
  let buffer = new [T](2);
  buffer[0] = value;
  buffer[1] = identity(value);
  return buffer;
}
fun main() => i32 {
  let box = new Box { value = 42 };
  let objects = allocate(box);
  let view: Value = box;
  let interfaces = allocate(view);
  let f: (i32) -> i32 = fn(value) { return value + 2; };
  let callbacks = allocate(f);
  let callback = callbacks[1];
  let strings = allocate("hello");
  let buffers = new [[i32]](1);
  let numbers = allocate(42);
  buffers[0] = numbers;
  if objects[1].value != 42 or interfaces[1].get() != 42 or buffers[0][1] != 42 { return 1; }
  let result = callback(40);
  free buffers, numbers, strings, callbacks, interfaces, objects;
  free f, box;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "dynamic-empty-buffer";
      source =
        "fun main() => i32 { let buffer = new [i32](0); free buffer; return \
         42; }";
      expected_exit = 42;
    };
    {
      name = "dynamic-negative-capacity";
      source =
        "fun main() => i32 { let count = -1; let buffer = new [i32](count); \
         free buffer; return 42; }";
      expected_exit = 1;
    };
    {
      name = "dynamic-overflow-capacity";
      source =
        "fun main() => i32 { let count: i64 = 9223372036854775807; let buffer \
         = new [i128](count); free buffer; return 42; }";
      expected_exit = 1;
    };
    {
      name = "returned-fixed-array-survives-clobber";
      source =
        {|@C fun clobber();
fun make<T>(value: T) => [T; 2] { return [value, value]; }
fun main() => i32 {
  let values = make(21);
  clobber();
  let result = values[0] + values[1];
  free values;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "nested-fixed-array-assignment";
      source =
        {|fun make() => [[i32; 2]; 2] { return [[20, 22], [1, 2]]; }
fun main() => i32 {
  let values = make();
  values[0] = values[0];
  values[1] = values[0];
  let result = values[1][0] + values[1][1];
  free values;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-functions-and-values";
      source =
        {|fun identity<T>(value: T) => T { return value; }
fun apply<T,U>(f: (T) -> U, value: T) => U { return f(value); }
fun main() => i32 {
  let f: (i32) -> i32 = identity;
  let x: i64 = identity(42);
  let s = identity("hello");
  let result = f(apply(identity, x as i32));
  free f;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-return-context-and-recursion";
      source =
        {|fun zero<T>() => T { return 0; }
fun left<T>(value: T, n: i32) => T {
  if n == 0 { return value; }
  return right(value, n - 1);
}
fun right<U>(value: U, n: i32) => U { return left(value, n); }
fun main() => i32 { let x: i64 = zero(); return left(42, 4) + (x as i32); }
|};
      expected_exit = 42;
    };
    {
      name = "generic-aggregate-signatures";
      source =
        {|class Box { let value: i32; }
fun identity<T>(value: T) => T { return value; }
fun first<T>(values: [T; 2]) => T { return values[0]; }
fun main() => i32 {
  let box = new Box { value = 42 };
  let boxes = [box, box];
  let result = identity(first(boxes)).value;
  free boxes;
  free box;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-array-values-and-callbacks";
      source =
        {|fun identity<T>(value: T) => T { return value; }
fun apply<T>(f: (T) -> T, value: T) => T { return f(value); }
fun first<T>(values: [T; 2]) => T { return values[0]; }
fun capture<T>(value: T) => () -> T {
  let copy: T = value;
  return fn[copy]() { return copy; };
}
fun main() => i32 {
  let values = [20, 22];
  let f: ([i32; 2]) -> [i32; 2] = identity;
  let direct = identity(values);
  let indirect = f(direct);
  let callback = apply(identity, indirect);
  let matrix = [[20, 22], [22, 20]];
  let nested = apply(identity, matrix);
  let closure = capture(callback);
  let captured = closure();
  let result = first(captured) + first(nested[1]);
  free values, matrix;
  free closure;
  free f;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-class-values-and-callbacks";
      source =
        {|class Box { let value: i32; }
fun identity<T>(value: T) => T { return value; }
fun apply<T>(f: (T) -> T, value: T) => T { return f(value); }
fun main() => i32 {
  let box = new Box { value = 42 };
  let f: (Box) -> Box = identity;
  let result = apply(identity, f(box)).value;
  free f;
  free box;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-lambda-types-and-side-effects";
      source =
        {|let count = 0;
fun next() => i32 { count += 1; return 40; }
fun identity<T>(value: T) => T { return value; }
fun capture<T>(value: T) => () -> T {
  let copy: T = value;
  return fn[copy]() { return copy; };
}
fun main() => i32 {
  let f = capture(identity(next()));
  let result = f() + count + count;
  free f;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "generic-void-and-inline";
      source =
        {|let count = 0;
inline fun identity<T>(value: T) => T { return value; }
fun consume<T>(value: T) { count += 1; }
fun main() => i32 { consume(1); consume("hello"); return identity(40) + count; }
|};
      expected_exit = 42;
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

let test_fixture ?(optimization = "-O0") fixture test_context =
  check_frontend_stages fixture;
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir
    ~prefix:("blink-" ^ fixture.name ^ "-")
    test_context
    (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:fixture.source;
      let compile_command =
        Printf.sprintf "%s %s %s" (Filename.quote compiler) optimization
          (Filename.quote "program.bl")
      in
      Native_test_support.assert_success_silently "Blink compiler"
        compile_command;
      Native_test_support.assert_success "LLVM verification"
        "llc --filetype=null new_output.ll -o /dev/null";
      if fixture.name = "returned-fixed-array-survives-clobber" then (
        Core.Out_channel.write_all "clobber.c"
          ~data:
            "__attribute__((noinline)) void clobber(void) { volatile unsigned \
             char bytes[16384]; for (unsigned i = 0; i < sizeof bytes; ++i) \
             bytes[i] = (unsigned char)i; }";
        (if optimization = "-O0" then
           let ir = Core.In_channel.read_all "new_output.ll" in
           assert_bool "returned literal uses heap allocation"
             (String.split_on_char '\n' ir
             |> List.exists (fun line ->
                 Core.String.is_substring line ~substring:"call"
                 && (Core.String.is_substring line ~substring:"@malloc("
                    || Core.String.is_substring line ~substring:"@calloc("))));
        Native_test_support.compile_and_run_with_sources
          ~sources:[ "clobber.c" ] ~expected_exit:fixture.expected_exit)
      else
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
         "generic native execution"
         >::: List.concat_map
                (fun fixture ->
                  List.map
                    (fun optimization ->
                      fixture.name ^ optimization
                      >:: test_fixture ~optimization fixture)
                    [ "-O0"; "-O2" ])
                generic_fixtures;
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
