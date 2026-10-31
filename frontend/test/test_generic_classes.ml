open OUnit2
module T = Typing.Typed_ast
module D = Desugaring.Desugared_ast

let parse source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = "generic-classes.bl" };
  match Parsing.Parse.parse_prog lexbuf with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let typed source =
  match Typing.Type.type_prog (parse source) with
  | Ok program -> program
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let positive name source = name >:: fun _ -> ignore (typed source)

let negative name expected source =
  name >:: fun _ ->
  match Typing.Type.type_prog (parse source) with
  | Ok _ -> assert_failure "Expected a type error"
  | Error error ->
      let message = Core.Error.to_string_hum error in
      List.iter
        (fun substring ->
          assert_bool message (Core.String.is_substring message ~substring))
        [ "Type Error:"; "generic-classes.bl:"; expected ];
      assert_bool message
        (not
           (Core.String.is_substring message
              ~substring:"Internal Typechecker Error"))

let box = "class Box<T> { let value: T; fun get() => T { return value; } }\n"

let suite =
  "generic classes"
  >::: [
         ( "parser binds class parameters everywhere and retains locations"
         >:: fun _ ->
           match
             parse
               "class Box<Element> { let value: Element; fun copy(x: Element) \
                => Box<Element> { let y: Element = x; return new Box<Element> \
                { value = y }; } }"
           with
           | Ast.Prog (_, [ { elt = { declaration = Ast.Class cn; _ }; _ } ]) ->
               assert_equal [ "Element" ]
                 (List.map
                    (fun (p : Ast.type_param) -> p.pname.elt)
                    cn.elt.cparams);
               assert_bool "parameter location"
                 ((List.hd cn.elt.cparams).pname.loc <> Util.Range.norange);
               (match (List.hd cn.elt.fields).elt with
               | _, Some (Ast.TParam p), _, _ -> assert_equal "Element" p.elt
               | _ -> assert_failure "field parameter not bound");
               let method_ = List.hd cn.elt.methods in
               (match (method_.elt.frtyp, List.rev method_.elt.body) with
               | ( Ast.RetVal (Ast.TRef (Ast.RGeneric (_, [ Ast.TParam _ ]))),
                   {
                     elt =
                       Ast.Ret
                         (Some
                            {
                              elt =
                                Ast.ObjInit
                                  ( {
                                      elt = Ast.RGeneric (_, [ Ast.TParam _ ]);
                                      loc;
                                    },
                                    _ );
                              _;
                            });
                     _;
                   }
                   :: _ ) ->
                   assert_bool "initializer location" (loc <> Util.Range.norange)
               | _ -> assert_failure "dependent class type not bound");
               ignore (Ast.show_prog (parse box))
           | _ -> assert_failure "expected a class" );
         positive
           "multiple parameters, nested types, arrays, and function types"
           (box
          ^ "class Pair<A, B> { let first: A; let second: B; } fun read(p: \
             Pair<Box<i32>, [i32; 2]>) => i32 { return p.first.get() + \
             p.second[0]; } fun main() => i32 { let x = new Box<i32> { value = \
             40 }; let y = new Pair<Box<i32>, [i32; 2]> { first = x, second = \
             [2, 0] }; return read(y); }");
         positive "adjacent closing angles and assignment"
           (box
          ^ "fun main() => i32 { let x: Box<Box<i32>>=new Box<Box<i32>> { \
             value = new Box<i32> { value = 42 } }; return x.value.value; }");
         positive "function type arguments in method signatures"
           "class Token<F> { fun apply(f: F) => F { return f; } } fun main() \
            => i32 { let t = new Token<(i32) -> i32> {}; let f: (i32) -> i32 = \
            fn[](x) { return x; }; return t.apply(f)(42); }";
         positive
           "generic prototypes match class patterns with renamed parameters"
           (box
          ^ "fun read<T>(x: Box<T>) => T; fun read<U>(x: Box<U>) => U { return \
             x.value; } fun main() => i32 { return read(new Box<i32> { value = \
             42 }); }");
         positive "expected result infers through class patterns"
           "class Token<T> {} fun make<T>() => Token<T> { return new Token<T> \
            {}; } fun main() => i32 { let x: Token<i32> = make(); free x; \
            return 0; }";
         negative "class template conflicts with interface" "already exists"
           "class C<T> {} interface C {}";
         negative "duplicate class template" "already exists"
           "class C<T> {} class C<U> {}";
         positive "recursive and mutually recursive fields"
           "class Left<T> { let next: Right<T> = null; } class Right<U> { let \
            next: Left<U> = null; } fun main() => i32 { let node = new \
            Left<i32> {}; free node; return 0; }";
         positive "generic functions infer through class applications"
           (box
          ^ "fun read<T>(box: Box<T>) => T { return box.get(); } fun main() => \
             i32 { return read(new Box<i32> { value = 42 }); }");
         positive "function specialization discovers class applications"
           (box
          ^ "fun wrap<T>(value: T) => Box<T> { return new Box<T> { value = \
             value }; } fun main() => i32 { let x = wrap(42); let y = \
             wrap(true); return x.get(); }");
         positive "constructor and typed default initialization"
           "class Box<T> { let value: T; fun Box() => Box<T> { let v: T; \
            return new Box<T> { value = v }; } } fun main() => i32 { let x: \
            Box<i32>; return x.value; }";
         positive "concrete interfaces and interface type arguments"
           ("interface I { fun value() => i32; } class Item impl I { fun \
             value() => i32 { return 42; } } " ^ box
          ^ "fun read<T>(x: Box<T>) => T { return x.value; } fun main() => i32 \
             { let x = new Box<I> { value = new Item {} }; return \
             read(x).value(); }");
         positive "generic class implements ordinary interface"
           "interface Counter { let count: i32; fun value() => i32; } class \
            C<T> impl Counter { let item: T; let count = 42; fun value() => \
            i32 { return count; } } fun main() => i32 { let x: Counter = new \
            C<bool> { item = true }; return x.value(); }";
         positive "typed lambda annotations and captures"
           (box
          ^ "fun wrap<T>(x: T) => Box<T> { let f: (T) -> Box<T> = fn[](y: T) \
             -> Box<T> { return new Box<T> { value = y }; }; return f(x); } \
             fun main() => i32 { return wrap(42).get(); }");
         positive "generic callbacks infer through class patterns"
           (box
          ^ "fun read<T>(x: Box<T>) => T { return x.value; } fun apply<A, \
             B>(f: (A) -> B, x: A) => B { return f(x); } fun main() => i32 { \
             return apply(read, new Box<i32> { value = 42 }); }");
         positive
           "ordinary required reference field needs no default constructor"
           "class Item { let value: i32; } class Holder { let item: Item; } \
            fun main() => i32 { let x = new Holder { item = new Item { value = \
            42 } }; return x.item.value; }";
         negative "required reference field is still mandatory" "Missing fields"
           "class Item {} class Holder { let item: Item; } fun main() => i32 { \
            let x = new Holder {}; return 0; }";
         positive "unused dependent class bodies remain deferred"
           "class Bad<T> { fun f() => T { return missing; } } fun main() => \
            i32 { return 0; }";
         negative "bare class template" "requires type arguments"
           (box ^ "fun f(x: Box) {}");
         negative "arity" "expects 1 type arguments, got 2"
           (box ^ "fun f(x: Box<i32, bool>) {}");
         negative "ordinary class application"
           "Not a generic class or interface" "class C {} fun f(x: C<i32>) {}";
         negative "interface application" "Not a generic class or interface"
           "interface I {} fun f(x: I<i32>) {}";
         negative "duplicate parameters" "Duplicate type parameter"
           "class C<T, T> {}";
         negative "unknown argument even if unused" "class undefined"
           "class C<T> {} fun f(x: C<Missing>) {}";
         negative "invalid array argument even if unused" "too large"
           "class C<T> {} fun f(x: C<[i32; \
            999999999999999999999999999999999999]>) {}";
         negative "invariant specializations"
           "Expected Box<i64> but got Box<i32>"
           (box
          ^ "fun f(x: Box<i64>) {} fun main() => i32 { f(new Box<i32> { value \
             = 1 }); return 0; }");
         negative "generic method remains unsupported" "Generic methods"
           "class C<T> { fun f<U>(x: U) => U { return x; } }";
         negative "invalid dependent body is checked on use" "Missing fields"
           "class C<T> { let x: T; fun copy() => C<T> { return new C<T> {}; } \
            } fun f(x: C<i32>) {}";
         negative "unbounded specialization" "specialization limit exceeded"
           "class C<T> { let next: C<[T; 1]> = null; } fun f(x: C<i32>) {}";
         ( "specializations are cached and lowered as concrete classes"
         >:: fun _ ->
           let program =
             typed
               (box
              ^ "fun main() => i32 { let x = new Box<i32> { value = 42 }; let \
                 y: Box<i32> = x; let z = new Box<bool> { value = true }; \
                 return y.get(); }")
           in
           let (T.Prog (_, _, classes, _, _, _)) = program in
           assert_equal 2 (List.length classes);
           assert_equal 2
             (List.length
                (List.sort_uniq String.compare
                   (List.map (fun (c : T.cdecl) -> c.cname) classes)));
           match Desugaring.Desugar.desugar_prog program with
           | Error error -> assert_failure (Core.Error.to_string_hum error)
           | Ok (D.Prog (_, _, classes, _, _, _)) ->
               assert_equal 2 (List.length classes);
               List.iter (fun (c : D.cdecl) -> assert_equal [] c.impls) classes
         );
         ( "compilation state is isolated" >:: fun _ ->
           ignore (typed (box ^ "fun f(x: Box<i32>) {}"));
           let (T.Prog (_, _, classes, _, _, _)) =
             typed (box ^ "fun f(x: Box<bool>) {}")
           in
           assert_equal 1 (List.length classes) );
       ]
