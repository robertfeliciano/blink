open OUnit2

type fixture = { name : string; source : string; expected_exit : int }

let fixtures =
  [
    {
      name = "free-operands-evaluate-in-source-order";
      source =
        {|class Item {}
class Counter { let order: i32 = 0; }
fun pick(counter: Counter, item: Item, digit: i32) => Item {
  counter.order = counter.order * 10 + digit;
  return item;
}
fun main() => i32 {
  let counter = new Counter {};
  let left = new Item {}; let right = new Item {};
  let captured = 42;
  let callback: (i32) -> i32 = fn[captured](value) { return captured + value; };
  free pick(counter, left, 1), callback, pick(counter, right, 2);
  let order = counter.order;
  free counter;
  if order == 12 { return 42; }
  return 91;
}|};
      expected_exit = 42;
    };
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

let test_fixture ?(optimization = "-O0") fixture test_context =
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
      name = "interface-mutable-reference-fields-rebind-original-storage";
      source =
        {|@C fun strcmp(left: string, right: string) => i32;
interface Child { let value: i32; }
class Leaf impl Child { let value = 0; }
class Box { let value = 0; }
interface Mutable {
  let name: string;
  let values: [i32; 2];
  let box: Box;
  let child: Child;
}
class Storage impl Mutable {
  let child: Child = null;
  let box: Box = null;
  let values: [i32; 2] = [0, 0];
  let name = "old";
}
fun replace(target: Mutable, name: string, values: [i32; 2], box: Box,
            child: Child) => void {
  target.name = name;
  target.values = values;
  target.box = box;
  target.child = child;
}
fun main() => i32 {
  let storage = new Storage {}; let target: Mutable = storage;
  let box = new Box {}; let leaf = new Leaf {}; let values = [0, 0];
  replace(target, "new", values, box, leaf);
  if strcmp(storage.name, "new") != 0 { return 1; }
  if storage.box != box { return 2; }
  target.box.value = 20;
  target.child.value = 21;
  target.values[1] = 1;
  if storage.child.value != 21 or storage.values[1] != 1 { return 3; }
  if values[1] != 1 { return 4; }
  let result = box.value + leaf.value + values[1];
  free storage, box, leaf;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-fields-use-concrete-layout-and-method-slots";
      source =
        {|@C fun strcmp(left: string, right: string) => i32;
interface State {
  let count: i32;
  let enabled: bool;
  let weight: f64;
  let ratio: f32;
  const label: string;
  fun value() => i32;
}
class First impl State {
  let enabled = false;
  let count = 10;
  const label = "first";
  let ratio: f32 = 0.0;
  let weight: f64 = 0.5;
  fun value() => i32 { return count + (weight as i32); }
}
class Second impl State {
  let weight: f64 = 0.5;
  let label = "second";
  let ratio: f32 = 0.0;
  let count = 10;
  let enabled = false;
  fun value() => i32 { return count + (weight as i32); }
}
fun update(item: State) => i32 {
  item.count += 10;
  item.enabled = not item.enabled;
  item.weight += 0.5;
  item.ratio = 1.25;
  return item.value();
}
fun main() => i32 {
  let first = new First {}; let second = new Second {};
  let left: State = first; let right: State = second;
  let result = update(left) + update(right);
  if first.count != 20 or second.count != 20 { return 1; }
  if not first.enabled or not right.enabled { return 2; }
  if left.weight != 1.0 or second.ratio != 1.25 { return 3; }
  if strcmp(left.label, "first") != 0 { return 4; }
  if strcmp(right.label, "second") != 0 { return 5; }
  free first, second;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-field-only-nested-references-and-shallow-const";
      source =
        {|interface Child { let value: i32; }
class Leaf impl Child { let value = 10; }
class Box { let count = 10; }
interface Holder {
  const box: Box;
  const peer: Child;
  const values: [i32; 2];
  let selected: Child;
}
class Storage impl Holder {
  let selected: Child;
  const values: [i32; 2] = [1, 2];
  const peer: Child = null;
  const box: Box = new Box {};
}
fun main() => i32 {
  let box = new Box {}; let leaf = new Leaf {};
  let peer: Child = leaf;
  let storage = new Storage { box = box, peer = peer, selected = peer };
  let holder: Holder = storage;
  holder.box.count += 10;
  holder.peer.value += 5;
  holder.values[1] += 5;
  let replacement = new Leaf { value = 42 };
  holder.selected = replacement;
  if box.count != 20 or leaf.value != 15 { return 1; }
  if storage.values[1] != 7 { return 2; }
  if storage.selected.value != 42 { return 3; }
  let result = holder.selected.value;
  free storage, box, leaf, replacement;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-field-receiver-index-and-rhs-evaluated-once";
      source =
        {|let order = 0;
class Box { let count = 10; }
interface State { let count: i32; const values: [i32; 2]; let box: Box; }
class Storage impl State {
  let count = 0; const values: [i32; 2] = [0, 0]; let box = new Box {};
}
fun receiver(item: State) => State { order = order * 10 + 1; return item; }
fun index() => i32 { order = order * 10 + 2; return 1; }
fun rhs() => i32 { order = order * 10 + 3; return 7; }
fun makeFn(item: State) => () -> i32 {
  item.count = 100; item.values[1] = 100;
  return fn[]() { return 2; };
}
fun swapBox(item: State, other: Box) => () -> i32 {
  item.box = other;
  return fn[]() { return 2; };
}
fun main() => i32 {
  let storage = new Storage {}; let item: State = storage;
  receiver(item).count = rhs();
  if order != 13 or storage.count != 7 { return 1; }
  order = 0;
  receiver(item).count += rhs();
  if order != 13 or storage.count != 14 { return 2; }
  order = 0;
  receiver(item).values[index()] += rhs();
  if order != 123 or storage.values[1] != 7 { return 3; }
  let get: () -> State = fn[item]() { return receiver(item); };
  order = 0;
  get().count += rhs();
  if order != 13 or storage.count != 21 { return 4; }
  order = 0;
  get().values[index()] = rhs();
  if order != 123 or storage.values[1] != 7 { return 5; }
  storage.count = 10; order = 0;
  get().count += makeFn(item)();
  if order != 1 or storage.count != 12 { return 6; }
  storage.values[1] = 7; order = 0;
  get().values[index()] += makeFn(item)();
  if order != 12 or storage.values[1] != 9 { return 7; }
  let original = storage.box; let other = new Box { count = 100 };
  order = 0;
  (true ? get().box : get().box).count += swapBox(item, other)();
  if order != 1 or original.count != 12 or other.count != 100 { return 8; }
  free get, original, other, storage;
  return 42;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-generic-values-and-global-methods";
      source =
        {|const base: i32 = 40;
interface I { fun value() => i32; }
class C impl I { fun value() => i32 { return base + 2; } }
fun identity<T>(value: T) => T { return value; }
fun main() => i32 {
  let item: I = new C {};
  if identity(item).value() != 42 { return 1; }
  let callback: (I) -> I = identity;
  let echoed = callback(item);
  let result = echoed.value();
  free callback, echoed;
  return result;
}|};
      expected_exit = 42;
    };
    {
      name = "interface-overridden-field-defaults";
      source =
        {|@C fun srand(seed: u32) => void;
@C fun rand() => i32;
interface I { fun echo(other: I) => I; fun value() => i32; }
class C impl I {
  let n: i32 = 0;
  fun echo(other: I) => I { return other; }
  fun value() => i32 { return n; }
}
fun make() => I { rand(); return new C { n = 20 }; }
class Holder { let item: I = make().echo(new C { n = 22 }); }
class ScalarHolder { let value: i32 = make().value(); }
fun main() => i32 {
  srand(123); let first = rand(); let second = rand(); srand(123);
  let c = new C { n = 42 };
  let holder = new Holder { item = c };
  let scalar = new ScalarHolder { value = 42 };
  if rand() != first { return 91; }
  if holder.item.value() != 42 or scalar.value != 42 { return 92; }
  srand(123); let default_holder = new Holder {};
  if rand() != second or default_holder.item.value() != 22 { return 93; }
  srand(123); let default_scalar = new ScalarHolder {};
  if rand() != second or default_scalar.value != 20 { return 94; }
  free default_holder.item;
  free c, holder, scalar, default_holder, default_scalar;
  return 42;
}
|};
      expected_exit = 42;
    };
    {
      name = "concrete-method-statements";
      source =
        {|class Box {
  let value: i32 = 0;
  fun set(first: i32, second: i32) => void { value = first + second; }
  fun add(amount: i32) => i32 { value += amount; return value; }
}
fun main() => i32 {
  let box = new Box {};
  box.set(10, 20);
  box.add(12);
  let result = box.value; free box; return result;
}
|};
      expected_exit = 42;
    };
    {
      name = "interface-while-changing-argument";
      source =
        {|interface Test { fun below(value: i32) => bool; }
class Limit impl Test {
  let maximum: i32 = 3; let checks: i32 = 0;
  fun below(value: i32) => bool { checks += 1; return value < maximum; }
}
fun main() => i32 {
  let limit = new Limit {}; let test: Test = limit; let value = 0;
  while test.below(value) {
    value += 1;
    if value > 5 { return 91; }
  }
  let valid = value == 3 and limit.checks == 4;
  free limit;
  if valid { return 42; } return 92;
}
|};
      expected_exit = 42;
    };
    {
      name = "interface-while-changing-receiver";
      source =
        {|interface Test { fun below(value: i32) => bool; }
class Limit impl Test {
  let maximum: i32 = 0; let checks: i32 = 0;
  fun below(value: i32) => bool { checks += 1; return value < maximum; }
}
fun main() => i32 {
  let yes = new Limit { maximum = 10 }; let no = new Limit {};
  let test: Test = yes; let value = 0;
  while test.below(0) {
    value += 1; test = no;
    if value > 3 { return 91; }
  }
  let valid = value == 1 and yes.checks == 1 and no.checks == 1;
  free yes, no;
  if valid { return 42; } return 92;
}
|};
      expected_exit = 42;
    };
    {
      name = "interface-while-nested-continue";
      source =
        {|interface Test { fun below(value: i32) => bool; }
class Limit impl Test {
  let maximum: i32 = 0; let checks: i32 = 0;
  fun below(value: i32) => bool { checks += 1; return value < maximum; }
}
fun main() => i32 {
  let outer_limit = new Limit { maximum = 2 };
  let inner_limit = new Limit { maximum = 3 };
  let outer_test: Test = outer_limit; let inner_test: Test = inner_limit;
  let outer = 0; let total = 0;
  while outer_test.below(outer) {
    outer += 1; let inner = 0;
    if outer > 4 { return 91; }
    while inner_test.below(inner) {
      inner += 1;
      if inner > 5 { return 92; }
      if inner == 2 { continue; }
      total += inner;
    }
    continue;
  }
  let valid = outer == 2 and total == 8 and outer_limit.checks == 3 and inner_limit.checks == 8;
  free outer_limit, inner_limit;
  if valid { return 42; } return 93;
}
|};
      expected_exit = 42;
    };
    {
      name = "while-lambda-condition-captures-current-value";
      source =
        {|fun evaluate(test: () -> bool) => bool {
  let result = test(); free test; return result;
}
fun main() => i32 {
  let value = 0;
  while evaluate(fn[value]() -> bool { return value < 3; }) {
    value += 1;
    if value > 5 { return 91; }
    continue;
  }
  if value == 3 { return 42; } return 92;
}
|};
      expected_exit = 42;
    };
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
                (* The typing suite owns the full arity matrix. These cases
                   check CLI diagnostics and that failure prevents IR emission. *)
                (List.map
                   (fun name ->
                     (name, List.assoc name Call_arity_fixtures.underapplication))
                   [ "named-value-zero-expression"; "named-void-one-statement" ]);
         "interface runtime dispatch"
         >::: List.concat_map
                (fun fixture ->
                  List.map
                    (fun optimization ->
                      fixture.name ^ optimization
                      >:: test_fixture ~optimization fixture)
                    [ "-O0"; "-O2" ])
                interface_fixtures;
         "generic native execution"
         >::: List.concat_map
                (fun fixture ->
                  List.map
                    (fun optimization ->
                      fixture.name ^ optimization
                      >:: test_fixture ~optimization fixture)
                    [ "-O0"; "-O2" ])
                generic_fixtures;
         "native execution" >::: executable_tests;
       ]

let () = run_test_tt_main suite
