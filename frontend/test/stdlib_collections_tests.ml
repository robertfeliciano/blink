open OUnit2
open Native_test_support

let compiler = executable_path "../src/blink.exe"
let stdlib_root = executable_path "../../../../runtime/stdlib"

let imports =
  "import std.collections.list as lists;\n\
   import std.collections.linked_list as linked;\n"

let invariant =
  {|fun valid<T>(items: linked.LinkedList<T>) => bool {
  if items.size() == 0 { return items.head == null; }
  let first = items.head;
  if first == null { return false; }
  let current = first;
  let visited: i64 = 0;
  while visited < items.size() {
    if current == null { return false; }
    if current.next == null or current.prev == null { return false; }
    if current.next.prev != current or current.prev.next != current { return false; }
    if visited > 0 and current == first { return false; }
    current = current.next;
    visited += 1;
  }
  if current != first { return false; }
  visited = 0;
  while visited < items.size() {
    current = current.prev;
    visited += 1;
  }
  return current == first;
}
|}

let command optimization =
  String.concat " "
    (List.map Filename.quote
       [ compiler; optimization; "-stdlib-root"; stdlib_root; "main.bl" ])

let native name source expected_exit =
  List.map
    (fun optimization ->
      name ^ optimization >:: fun context ->
      in_temp_dir ~prefix:"blink-collections" context (fun () ->
          Core.Out_channel.write_all "main.bl"
            ~data:(imports ^ invariant ^ source);
          assert_success_silently "stdlib collections compilation"
            (command optimization);
          compile_and_run ~expected_exit))
    [ "-O0"; "-O2" ]

let fixtures =
  [
    ( "empty singleton end operations and reuse",
      {|fun main() => i32 {
  let items = new linked.LinkedList<i32> {};
  if not items.isEmpty() or items.size() != 0 or not valid(items) { return 1; }
  items.clear(); items.clear();
  items.addFirst(20);
  if items.head.next != items.head or items.head.prev != items.head { return 2; }
  if items.getFirst() != 20 or items.getLast() != 20 or not valid(items) { return 3; }
  items.addLast(22);
  if items.getFirst() != 20 or items.getLast() != 22 or not valid(items) { return 4; }
  if items.removeFirst() != 20 or not valid(items) { return 5; }
  if items.removeLast() != 22 or not valid(items) { return 6; }
  items.addLast(42);
  if items.removeFirst() != 42 or not items.isEmpty() or not valid(items) { return 7; }
  items.addFirst(42);
  if items.removeLast() != 42 or not valid(items) { return 8; }
  items.add(42);
  items.clear(); items.clear();
  if not items.isEmpty() or not valid(items) { return 9; }
  items.addLast(42);
  let result = items.getFirst();
  items.clear(); free items;
  return result;
}|}
    );
    ( "indexed insert replace remove and interface inference",
      {|fun first<T>(items: lists.List<T>) => T { return items.get(0); }
fun main() => i32 {
  let concrete = new linked.LinkedList<i32> {};
  let items: lists.List<i32> = concrete;
  items.insert(0, 10);
  items.insert(1, 30);
  items.insert(1, 20);
  items.insert(0, 0);
  items.insert(4, 40);
  if items.size() != 5 or not valid(concrete) { return 1; }
  let index = 0;
  while index < 5 {
    if items.get(index) != index * 10 { return 2; }
    index += 1;
  }
  if items.set(2, 22) != 20 or items.get(2) != 22 { return 3; }
  if items.removeAt(1) != 10 or not valid(concrete) { return 4; }
  if items.removeAt(3) != 40 or not valid(concrete) { return 5; }
  if items.removeAt(0) != 0 or first(concrete) != 22 or not valid(concrete) { return 6; }
  if items.removeAt(1) != 30 or not valid(concrete) { return 7; }
  let result = items.removeAt(0) + 20;
  if not items.isEmpty() or not valid(concrete) { return 8; }
  items.clear(); free items;
  return result;
}|}
    );
    ( "model sequence preserves both links after every mutation",
      {|fun main() => i32 {
  let items = new linked.LinkedList<i32> {};
  let expected: [i32; 64];
  let count = 0; let seed = 1; let step = 0;
  while step < 300 {
    seed = (seed * 17 + 11) % 97;
    let operation = seed % 6;
    if count == 0 or (operation < 3 and count < 32) {
      let index = seed % (count + 1);
      let cursor = count;
      while cursor > index { expected[cursor] = expected[cursor - 1]; cursor -= 1; }
      expected[index] = step;
      items.insert(index, step);
      count += 1;
    } else {
      let index = seed % count;
      if operation == 3 {
        if items.set(index, step) != expected[index] { return 1; }
        expected[index] = step;
      } else {
        if items.removeAt(index) != expected[index] { return 2; }
        let cursor = index;
        while cursor + 1 < count { expected[cursor] = expected[cursor + 1]; cursor += 1; }
        count -= 1;
      }
    }
    if items.size() != count or not valid(items) { return 3; }
    let index = 0;
    while index < count {
      if items.get(index) != expected[index] { return 4; }
      index += 1;
    }
    step += 1;
  }
  items.clear();
  if not valid(items) { return 5; }
  free items;
  return 42;
}|}
    );
    ( "clear replacement and removal retain caller owned payloads",
      {|class Item { let value: i32 = 0; }
fun main() => i32 {
  let first = new Item { value = 20 };
  let second = new Item { value = 22 };
  let items = new linked.LinkedList<Item> {};
  items.add(first); items.add(second);
  if items.removeFirst() != first { return 1; }
  if items.set(0, first) != second { return 2; }
  items.clear();
  if not valid(items) { return 3; }
  let result = first.value + second.value;
  items.add(null);
  if items.isEmpty() or items.get(0) != null { return 4; }
  if items.removeAt(0) != null or not valid(items) { return 5; }
  items.clear(); free items, first, second;
  return result;
}|}
    );
    ( "interface aggregates arrays strings and bool payloads",
      {|@C fun strcmp(left: string, right: string) => i32;
fun main() => i32 {
  let inner = new linked.LinkedList<i32> {};
  let outer = new linked.LinkedList<lists.List<i32>> {};
  outer.add(inner);
  outer.getFirst().add(42);
  if not valid(outer) { return 1; }
  outer.clear();
  if inner.getFirst() != 42 { return 2; }
  let array = new [20, 22];
  let arrays = new linked.LinkedList<[i32; 2]> {};
  arrays.add(array);
  let removed = arrays.removeLast();
  if removed[0] + removed[1] != 42 or not valid(arrays) { return 3; }
  let strings = new linked.LinkedList<string> {};
  strings.add("first"); strings.addFirst("second");
  if strcmp(strings.removeFirst(), "second") != 0 { return 4; }
  let flags = new linked.LinkedList<bool> {};
  flags.add(true); flags.addLast(false);
  if not flags.removeFirst() or flags.removeLast() { return 5; }
  let result = inner.getFirst();
  inner.clear(); outer.clear(); arrays.clear(); strings.clear(); flags.clear();
  free inner, outer, arrays, strings, flags, array;
  return result;
}|}
    );
    ( "large ring clear and reuse",
      {|fun main() => i32 {
  let items = new linked.LinkedList<i32> {};
  let index = 0;
  while index < 1024 { items.addLast(index); index += 1; }
  if not valid(items) or items.get(0) != 0 or items.get(1023) != 1023 { return 1; }
  if items.get(511) != 511 or items.get(512) != 512 { return 2; }
  index = 0;
  while index < 512 {
    if items.removeFirst() != index { return 3; }
    index += 1;
  }
  if not valid(items) { return 4; }
  items.clear(); items.clear();
  if not valid(items) { return 5; }
  items.insert(0, 42);
  let result = items.removeLast();
  if not valid(items) { return 6; }
  free items;
  return result;
}|}
    );
  ]

let invalid_operations =
  [
    ("get negative", "items.add(42); let ignored = items.get(-1);");
    ("get upper bound", "items.add(42); let ignored = items.get(1);");
    ( "get huge index",
      "items.add(42); let ignored = items.get(9223372036854775807);" );
    ("set negative", "items.add(42); let ignored = items.set(-1, 0);");
    ("set upper bound", "items.add(42); let ignored = items.set(1, 0);");
    ("remove negative", "items.add(42); let ignored = items.removeAt(-1);");
    ("remove upper bound", "items.add(42); let ignored = items.removeAt(1);");
    ("insert negative", "items.insert(-1, 42);");
    ("insert past end", "items.add(42); items.insert(2, 42);");
    ("get first empty", "let ignored = items.getFirst();");
    ("get last empty", "let ignored = items.getLast();");
    ("remove first empty", "let ignored = items.removeFirst();");
    ("remove last empty", "let ignored = items.removeLast();");
    ("remove empty by index", "let ignored = items.removeAt(0);");
  ]

let type_error name source expected =
  name >:: fun context ->
  in_temp_dir ~prefix:"blink-collections-types" context (fun () ->
      Core.Out_channel.write_all "main.bl" ~data:(imports ^ source);
      assert_exit_code 1
        (command "-O0" ^ " > compiler.stdout 2> compiler.stderr");
      let message = Core.In_channel.read_all "compiler.stderr" in
      assert_bool message (Core.String.is_substring message ~substring:expected);
      assert_bool "failed compilation emits no IR"
        (not (Sys.file_exists "new_output.ll")))

let suite =
  "stdlib collections"
  >::: List.concat
         [
           List.concat_map
             (fun (name, source) -> native name source 42)
             fixtures;
           List.concat_map
             (fun (name, operation) ->
               native
                 ("invalid operation: " ^ name)
                 ("fun main() => i32 { let items = new linked.LinkedList<i32> \
                   {}; " ^ operation ^ " return 42; }")
                 1)
             invalid_operations;
           [
             type_error "element types are enforced"
               "fun main() => i32 { let items = new linked.LinkedList<i32> {}; \
                items.add(true); return 0; }"
               "i32";
             type_error "list specializations are invariant"
               "fun main() => i32 { let items: lists.List<i64> = new \
                linked.LinkedList<i32> {}; return 0; }"
               "Expected implementing class";
             type_error "node class is private"
               "fun main() => i32 { let node = new linked.Node<i32> { value = \
                42 }; return 0; }"
               "is private";
           ];
         ]

let () = run_test_tt_main suite
