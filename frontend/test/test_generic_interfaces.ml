open OUnit2
module T = Typing.Typed_ast
module D = Desugaring.Desugared_ast

let parse source =
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = "generic-interfaces.bl" };
  match Parsing.Parse.parse_prog lexbuf with
  | Ok p -> p
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let typed source =
  match Typing.Type.type_prog (parse source) with
  | Ok p -> p
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let positive name source = name >:: fun _ -> ignore (typed source)

let assert_negative expected source =
  match Typing.Type.type_prog (parse source) with
  | Ok _ -> assert_failure "Expected a type error"
  | Error e ->
      let message = Core.Error.to_string_hum e in
      List.iter
        (fun substring ->
          assert_bool message (Core.String.is_substring message ~substring))
        [ "Type Error:"; "generic-interfaces.bl:"; expected ];
      List.iter
        (fun substring ->
          assert_bool message
            (not (Core.String.is_substring message ~substring)))
        [ "Internal Typechecker Error"; "_BLI"; "_BLC" ]

let negative name expected source =
  name >:: fun _ -> assert_negative expected source

let value = "interface Value<T> { const item: T; fun get() => T; }\n"

let box =
  "class Box<T> impl Value<T> { let item: T; fun get() => T { return item; } }\n"

let read = "fun read<T>(x: Value<T>) => T { return x.get(); }\n"

let constraints_fixture () =
  Core.In_channel.read_all
    (Filename.concat
       (Filename.dirname Sys.executable_name)
       "../../../test/fixtures/generic_constraints.bl")

let suite =
  "generic interfaces"
  >::: [
         ( "bounded specializations preserve types and reuse instances"
         >:: fun _ ->
           let program = typed (constraints_fixture ()) in
           let (T.Prog (_, functions, _, _, _, _)) = program in
           let identities =
             List.filter
               (fun (f : T.fdecl) ->
                 Core.String.is_prefix f.fname ~prefix:"_BLG8identity_")
               functions
           in
           assert_equal 2 (List.length identities);
           List.iter
             (fun (f : T.fdecl) ->
               assert_equal (T.RetVal (fst (List.hd f.args))) f.frtyp)
             identities;
           match Desugaring.Desugar.desugar_prog program with
           | Ok _ -> ()
           | Error error -> assert_failure (Core.Error.to_string_hum error) );
         positive "self bounds share nominal specialization identities"
           "interface Self<T> { fun same() => T; } class C impl Self<C> { fun \
            same() => C { return this; } } fun same<T impl Self<T>>(x: T) => T \
            { return x.same(); } fun f(x: C) => C { return same(x); }";
         negative "bounds require explicit implementation"
           "does not satisfy interface constraint"
           "interface I { fun get() => i32; } class C { fun get() => i32 { \
            return 42; } } fun f<T impl I>(x: T) => T { return x; } fun g(x: \
            C) => C { return f(x); }";
         negative "bounds retain invariant interface arguments"
           "does not satisfy interface constraint"
           (value ^ box
          ^ "fun f<T impl Value<i32>>(x: T) => T { return x; } fun g(x: \
             Box<bool>) => Box<bool> { return f(x); }");
         negative "function values check newly discovered bounds"
           "does not satisfy interface constraint"
           "interface I<U> {} fun f<T impl I<T>>(x: T) => T { return x; } fun \
            g() { let callback: (i32) -> i32 = f; }";
         ( "nominal bounds cover classes and interfaces" >:: fun _ ->
           List.iter
             (fun declaration ->
               assert_negative "does not satisfy interface constraint"
                 ("interface I {} " ^ declaration ^ " fun f(x: Holder<i32>) {}"))
             [ "class Holder<T impl I> {}"; "interface Holder<T impl I> {}" ] );
         ( "unused declarations validate bounds and prototype agreement"
         >:: fun _ ->
           List.iter
             (fun (expected, source) -> assert_negative expected source)
             [
               ("must be an interface", "class C {} class Holder<T impl C> {}");
               ("Unknown constraint type", "interface Holder<T impl Missing> {}");
               ( "expects 1 type arguments",
                 "interface I<T> {} fun f<T impl I>(x: T) {}" );
               ( "does not satisfy interface constraint",
                 "interface I {} interface B<T impl I> {} class Holder<T impl \
                  B<i32>> {}" );
               ( "prototype does not match",
                 "interface I {} fun f<T impl I>(x: T) => T; fun f<U>(x: U) => \
                  U { return x; }" );
             ] );
         ( "parser binds parameters in fields, signatures, and implementations"
         >:: fun _ ->
           match parse (value ^ box) with
           | Ast.Prog
               ( _,
                 [
                   { elt = { declaration = Ast.Interface i; _ }; _ };
                   { elt = { declaration = Ast.Class c; _ }; _ };
                 ] ) ->
               assert_equal [ "T" ]
                 (List.map
                    (fun (p : Ast.type_param) -> p.pname.elt)
                    i.elt.iparams);
               assert_bool "located parameter"
                 ((List.hd i.elt.iparams).pname.loc <> Util.Range.norange);
               (match (List.hd i.elt.fields).elt.ftyp with
               | Ast.TParam p -> assert_equal "T" p.elt
               | _ -> assert_failure "unbound field type");
               (match (List.hd i.elt.protos).elt.frtyp with
               | Ast.RetVal (Ast.TParam _) -> ()
               | _ -> assert_failure "unbound method type");
               (match c.elt.impls with
               | [
                { elt = Ast.TRef (Ast.RGeneric (_, [ Ast.TParam _ ])); loc };
               ] ->
                   assert_bool "located implementation"
                     (loc <> Util.Range.norange)
               | _ -> assert_failure "unbound implementation type");
               let printed = Ast.show_prog (parse (value ^ box)) in
               List.iter
                 (fun substring ->
                   assert_bool printed
                     (Core.String.is_substring printed ~substring))
                 [ "Value<T>"; "impls=[TRef(Value<" ]
           | _ -> assert_failure "expected interface and class" );
         positive "ordinary class implements specialized interface"
           (value
          ^ "class IntBox impl Value<i32> { let item = 42; fun get() => i32 { \
             return item; } } fun f() => i32 { let x: Value<i32> = new IntBox \
             {}; return x.get(); }");
         positive "concrete class inference and explicit interface view"
           (value ^ box ^ read
          ^ "fun f() => i32 { let x = new Box<i32> { item = 42 }; let view: \
             Value<i32> = x; return read(x) + read(view); }");
         positive "ordinary implementing class also infers interface argument"
           (value ^ read
          ^ "class IntBox impl Value<i32> { let item = 42; fun get() => i32 { \
             return item; } } fun f() => i32 { return read(new IntBox {}); }");
         positive "different generic interfaces infer independently"
           "interface Left<T> { fun left() => T; } interface Right<T> { fun \
            right() => T; } class C impl Left<i32>, Right<bool> { fun left() \
            => i32 { return 42; } fun right() => bool { return true; } } fun \
            l<T>(x: Left<T>) => T { return x.left(); } fun r<T>(x: Right<T>) \
            => T { return x.right(); } fun f() => i32 { let x = new C {}; if \
            r(x) { return l(x); } return 0; }";
         positive
           "multiple parameters infer through reordered implementation \
            arguments"
           "interface Pair<A, B> { fun first() => A; fun second() => B; } \
            class Swapped<X, Y> impl Pair<Y, X> { let x: X; let y: Y; fun \
            first() => Y { return y; } fun second() => X { return x; } } fun \
            first<A, B>(x: Pair<A, B>) => A { return x.first(); } fun f() => \
            i32 { return first(new Swapped<bool, i32> { x = true, y = 42 }); }";
         positive
           "inference follows interface mapping rather than class arguments"
           "interface Value<T> { fun get() => T; } class Pair<T> impl \
            Value<[T; 2]> { let items: [T; 2]; fun get() => [T; 2] { return \
            items; } } fun read<T>(x: Value<T>) => T { return x.get(); } fun \
            f() => i32 { let x = new Pair<i32> { items = [40, 2] }; return \
            read(x)[0]; }";
         positive "generic class contains interface applications"
           (value ^ box
          ^ "class Holder<T> { let view: Value<T>; } fun f() => i32 { let h = \
             new Holder<i32> { view = new Box<i32> { item = 42 } }; return \
             h.view.get(); }");
         positive "nested class interface and function arguments"
           "class Box<T> { let item: T; } interface Mapping<A, B> { fun \
            apply(x: A) => B; } class C impl Mapping<Box<i32>, (i32) -> i32> { \
            fun apply(x: Box<i32>) => (i32) -> i32 { return fn[x](n) { return \
            x.item + n; }; } } fun apply<A, B>(m: Mapping<A, B>, x: A) => B { \
            return m.apply(x); } fun f() => i32 { return apply(new C {}, new \
            Box<i32> { item = 40 })(2); }";
         positive "expected interface result infers generic factory"
           (value ^ box
          ^ "fun make<T>() => Value<T> { let item: T; return new Box<T> { item \
             = item }; } fun f() => i32 { let x: Value<i32> = make(); return \
             x.get(); }");
         positive
           "expected function signature specializes generic function value"
           (value ^ box ^ read
          ^ "fun f() => i32 { let callback: (Value<i32>) -> i32 = read; return \
             callback(new Box<i32> { item = 42 }); }");
         positive "generic callback inference through interface patterns"
           (value ^ box ^ read
          ^ "fun apply<A, B>(f: (A) -> B, x: A) => B { return f(x); } fun f() \
             => i32 { let x: Value<i32> = new Box<i32> { item = 42 }; return \
             apply(read, x); }");
         negative "callbacks retain exact function signatures" "Expected"
           (value ^ box ^ read
          ^ "fun apply<A, B>(f: (A) -> B, x: A) => B { return f(x); } fun f() \
             => i32 { return apply(read, new Box<i32> { item = 42 }); }");
         positive "generic prototypes allow renamed interface parameters"
           (value ^ box
          ^ "fun read<T>(x: Value<T>) => T; fun read<U>(x: Value<U>) => U { \
             return x.get(); } fun f() => i32 { return read(new Box<i32> { \
             item = 42 }); }");
         positive "self and mutually recursive interfaces"
           "interface Left<T> { let right: Right<T>; fun next() => Left<T>; } \
            interface Right<T> { let left: Left<T>; } fun f(x: Left<i32>) => \
            Right<i32> { return x.next().right; }";
         positive "recursive class and interface dependencies"
           "interface Link<T> { let node: Node<T>; } class Node<T> impl \
            Link<T> { let node: Node<T> = null; let view: Link<T> = null; } \
            fun f(x: Link<i32>) => Link<i32> { return x.node; }";
         positive "new specialization found while typing generic function body"
           (value ^ box
          ^ "fun wrap<T>(item: T) => Box<T> { let view: Value<T> = new Box<T> \
             { item = item }; return new Box<T> { item = view.get() }; } fun \
             f() => i32 { return wrap(42).get(); }");
         positive
           "interface-only discovery in a generic function rebuilds contexts"
           "interface Marker<T> {} fun identity<T>(x: T) => T { let marker: \
            Marker<T> = null; return x; } fun f() => i32 { return \
            identity(42); }";
         positive
           "generic constructor preserves implemented interface substitutions"
           (value
          ^ "class Box<T> impl Value<T> { let item: T; fun Box() => Box<T> { \
             let item: T; return new Box<T> { item = item }; } fun get() => T \
             { return item; } }" ^ read
          ^ "fun f() => i32 { let box: Box<i32>; return read(box); }");
         positive "mutable requirements and readonly views preserve rules"
           "interface State<T> { let item: T; } class C<T> impl State<T> { let \
            item: T; } fun set<T>(x: State<T>, v: T) { x.item = v; } fun f() \
            => i32 { let c = new C<i32> { item = 0 }; set(c, 42); return \
            c.item; }";
         positive "generic interface can itself be a generic argument"
           (value ^ box ^ read
          ^ "fun f() => i32 { let inner: Value<i32> = new Box<i32> { item = 42 \
             }; let outer = new Box<Value<i32>> { item = inner }; return \
             read(outer).get(); }");
         negative "bare template" "requires type arguments"
           (value ^ "fun f(x: Value) {}");
         negative "wrong arity" "expects 1 type arguments, got 2"
           (value ^ "fun f(x: Value<i32, bool>) {}");
         negative "duplicate type parameter" "Duplicate type parameter"
           "interface I<T, T> {}";
         negative "duplicate interface templates" "already exists"
           "interface I<T> {} interface I<U> {}";
         negative "template conflicts with class" "already exists"
           "interface I<T> {} class I {}";
         negative "ordinary interface cannot be applied"
           "Not a generic class or interface"
           "interface I {} fun f(x: I<i32>) {}";
         negative "unknown type argument even when unused" "class undefined"
           "interface I<T> {} fun f(x: I<Missing>) {}";
         negative "oversized array argument" "too large"
           "interface I<T> {} fun f(x: I<[i32; \
            999999999999999999999999999999]>) {}";
         negative "generic interface cannot be constructed"
           "Cannot construct interface"
           (value ^ "fun f() { let x = new Value<i32> {}; }");
         negative "class cannot be an impl target" "Unknown interface"
           "class C<T> {} class D impl C<i32> {}";
         negative "bare template in impl" "requires type arguments"
           (value ^ "class C impl Value {}");
         negative "duplicate specialization" "Duplicate implemented interface"
           "interface I<T> {} class C impl I<i32>, I<i32> {}";
         negative "conflicting specializations" "Conflicting specializations"
           "interface I<T> {} class C impl I<i32>, I<bool> {}";
         negative "conflicting dependent specializations"
           "Conflicting specializations"
           "interface I<T> {} class C<T> impl I<T>, I<bool> {} fun f(x: \
            C<i32>) {}";
         negative "missing method" "missing interface method"
           "interface I<T> { fun get() => T; } class C impl I<i32> {}";
         negative "wrong method result" "signature does not match"
           "interface I<T> { fun get() => T; } class C impl I<i32> { fun get() \
            => bool { return true; } }";
         negative "wrong field type" "field item type does not match"
           "interface I<T> { let item: T; } class C impl I<i32> { let item = \
            true; }";
         negative "mutable requirement rejects const" "must be mutable"
           "interface I<T> { let item: T; } class C impl I<i32> { const item = \
            42; }";
         negative "readonly requirement blocks assignment" "constant field item"
           (value ^ "fun f(x: Value<i32>) { x.item = 0; }");
         negative "function fields remain unsupported after substitution"
           "Function fields are not supported"
           "interface I<T> { let item: T; } fun f(x: I<(i32) -> i32>) {}";
         negative "generic interface methods remain unsupported"
           "Generic interface methods" "interface I<T> { fun f<U>(x: U) => T; }";
         negative "duplicate members in unused template"
           "Duplicate interface member"
           "interface I<T> { let x: T; fun x() => T; }";
         negative "annotated methods in unused template"
           "cannot have annotations" "interface I<T> { @C fun f() => T; }";
         negative "conflicting inference from another argument"
           "Conflicting types"
           (value ^ box
          ^ "fun choose<T>(x: Value<T>, other: T) => T { return other; } fun \
             f() { choose(new Box<i32> { item = 42 }, true); }");
         negative "invariant interfaces" "Expected"
           (value ^ "fun f(x: Value<i32>) => Value<i64> { return x; }");
         negative "no structural conformance" "specialization mismatch"
           (value ^ read
          ^ "class C { let item = 42; fun get() => i32 { return item; } } fun \
             f() => i32 { return read(new C {}); }");
         negative "nested class invariance is not weakened by inference"
           "Expected"
           (value ^ box
          ^ "class Holder<T> { let item: T; } fun f<T>(h: Holder<Value<T>>) => \
             T { return h.item.get(); } fun main() => i32 { return f(new \
             Holder<Box<i32>> { item = new Box<i32> { item = 42 } }); }");
         negative "expanding recursive specialization is bounded"
           "specialization limit exceeded"
           "interface I<T> { let next: I<[T; 1]>; } fun f(x: I<i32>) {}";
         ( "specializations are cached and lower as concrete interfaces"
         >:: fun _ ->
           let program =
             typed
               (value ^ box ^ read
              ^ "fun f() => i32 { let x = new Box<i32> { item = 42 }; let y: \
                 Value<i32> = x; let z: Value<bool> = new Box<bool> { item = \
                 true }; return read(x) + read(y); }")
           in
           let (T.Prog (_, _, classes, _, _, interfaces)) = program in
           assert_equal 2 (List.length interfaces);
           assert_equal 2 (List.length classes);
           assert_equal 2
             (List.length
                (List.sort_uniq String.compare
                   (List.map (fun (i : T.idecl) -> i.iname) interfaces)));
           match Desugaring.Desugar.desugar_prog program with
           | Error e -> assert_failure (Core.Error.to_string_hum e)
           | Ok (D.Prog (_, functions, classes, _, _, interfaces)) ->
               assert_equal 2 (List.length interfaces);
               List.iter
                 (fun (c : D.cdecl) ->
                   assert_equal 1 (List.length c.impls);
                   assert_bool "implementation references concrete declaration"
                     (List.exists
                        (fun (i : D.idecl) -> i.iname = (List.hd c.impls).iname)
                        interfaces))
                 classes;
               assert_bool "calls insert concrete interface conversions"
                 (List.exists
                    (fun (f : D.fdecl) ->
                      List.exists
                        (function
                          | D.Ret
                              (Some
                                 (D.Bop
                                    ( _,
                                      D.Call (_, [ D.InterfaceCast _ ], _),
                                      _,
                                      _ ))) ->
                              true
                          | _ -> false)
                        f.body)
                    functions) );
         ( "compilations do not share specialization state" >:: fun _ ->
           ignore (typed (value ^ "fun f(x: Value<i32>) {}"));
           let (T.Prog (_, _, _, _, _, interfaces)) =
             typed (value ^ "fun f(x: Value<bool>) {}")
           in
           assert_equal 1 (List.length interfaces);
           assert_equal T.TBool (List.hd (List.hd interfaces).fields).ftyp );
       ]
