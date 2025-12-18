%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc = Range.mk_lex_range startpos endpos }

%}

/* tokens */
%token EOF
%token <Z.t> INT
%token <float> FLOAT
%token NULL
%token <string> STRING
%token <string> IDENT

%token LPAREN    /* ( */
%token RPAREN    /* ) */
%token LBRACE    /* { */
%token RBRACE    /* } */
%token LBRACKET  /* [ */
%token RBRACKET  /* ] */
%token COMMA     /* , */
%token DOT       /* . */
%token RANGE     /* .. */
%token RANGE_INCL/* ..= */
%token COLON     /* : */
// %token COLCOL    /* :: */
%token SEMI      /* ; */
%token EQUAL     /* = */
%token ARROW     /* => */
%token THIN_ARROW/* -> */
%token PLUS      /* + */
%token MINUS     /* - */
%token MULT      /* * */
%token DIV       /* / */
%token AT        /* @ */
%token MOD       /* % */
%token POW       /* ** */
%token LTLT      /* << */
%token GTGT      /* >> */
%token GTGTGT    /* >>> */
%token CARET     /* ^ */
%token AMP       /* & */
%token BOR       /* | */
%token TILDE      /* ~ */
%token XOR       /* xor */
%token PLUEQ     /* += */
%token MINEQ     /* -= */
%token TIMEQ     /* *= */
%token DIVEQ     /* /= */
%token ATEQ      /* @= */
%token POWEQ     /* **= */
%token LTLTEQ    /* <<= */
%token GTGTEQ    /* >>= */
%token GTGTGTEQ  /* >>>= */
%token CARETEQ   /* ^= */
%token AMPEQ     /* &= */
%token BOREQ     /* |= */
%token LT        /* < */
%token LTE       /* <= */
%token GT        /* > */
%token GTE       /* >= */
%token LT_TYPE   /* < */
%token GT_TYPE   /* > */
// %token OPT_TYPE  /* ? */
%token NEQ       /* != */
%token EQEQ      /* == */
%token AND       /* and */
%token OR        /* or */
%token NOT       /* not */
%token LET       /* let */
%token NEW       /* new */
%token CONST     /* const */
%token TVOID     /* void */
/* integer tokens */
%token Ti8
%token Ti16
%token Ti32
%token Ti64
%token Ti128
%token Tu8
%token Tu16
%token Tu32
%token Tu64
%token Tu128
/* floating point tokens */
%token Tf32
%token Tf64
%token TSTRING   /* string */
%token TBOOL     /* bool */
%token FUN       /* fun */
%token IF        /* if */
%token IN        /* in */
%token ELSE      /* else */
%token FOR       /* for */
%token BY        /* by */
%token WHILE     /* while */
%token BREAK     /* break */
%token CONT      /* continue */
%token RETURN    /* return */
// %token STATIC    /* static */
%token TRUE      /* true */
%token FALSE     /* false */
// %token WHERE     /* where */
// %token IMPORT    /* import */
// %token ENABLE    /* enable */
%token CLASS     /* class */
%token DEL       /* del */
%token IMPLS     /* impls */
// %token GLOBAL    /* global */
// %token QMARK     /* ? */
%token AS        /* as */
%token BAR       /* | */
%token FN        /* fn */

// %right EQUAL PLUEQ MINEQ TIMEQ DIVEQ ATEQ POWEQ
%left OR
%left AND
%left XOR

%left EQEQ NEQ
%left LT LTE GT GTE

%left BOR
%left CARET
%left AMP

%left PLUS MINUS
%left MULT DIV MOD LTLT GTGT GTGTGT
%right POW

// %nonassoc TILDE

%left AT
// %left DOT
// %left RANGE RANGE_INCL
// %right THIN_ARROW
// %nonassoc LOW
// %nonassoc QMARK
// %nonassoc NOT

%start program

(* declare the important top-level symbol types for menhir/ocamlyacc; you usually
   get menhir's type inference via dune/--infer. *)
%type <Ast.program> program
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty

%%

(* -----------------------
   Top-level
   ----------------------- *)

tdecl:
  | f=fdecl { `Fun f }
  | c=cdecl { `Class c }
  | p=pdecl { `Proto p }

program:
  | decls=list(tdecl) EOF
      {
        let fdecls, cdecls, pdecls =
          List.fold_right
            (fun d (fs, cs, ps) ->
              match d with
              | `Fun f   -> (f :: fs, cs, ps)
              | `Class c -> (fs, c :: cs, ps)
              | `Proto p -> (fs, cs, p :: ps))
            decls
            ([], [], [])
        in
        Prog (fdecls, cdecls, pdecls)
      }

(* -----------------------
   Function, Prototype, & Class decls
   ----------------------- *)

annotation:
  | AT id=IDENT args=annotation_args?
      {
        let id_node =
          loc ($startpos(id)) ($endpos(id)) id
        in
        (id_node, args)
      }

annotation_args:
  | LPAREN l=separated_list(COMMA, exp) RPAREN { l }

fdecl_list:
  | /* empty */ { [] }
  | hd=fdecl tl=fdecl_list { hd :: tl }

fdecl:
  | annotations=list(annotation) FUN fname=IDENT LPAREN args=arg_list RPAREN frtyp=ret_ty_spec body=block
      { (loc $startpos $endpos { annotations; frtyp; fname; args; body }) }

pdecl:
  | annotations=list(annotation) FUN fname=IDENT LPAREN args=arg_list RPAREN frtyp=ret_ty_spec SEMI
      { (loc $startpos $endpos { annotations; frtyp; fname; args }) }

cdecl:
  | annotations=list(annotation) CLASS cname=IDENT impls=impls_spec LBRACE fields=field_list methods=fdecl_list RBRACE
      { (loc $startpos $endpos { annotations; cname; impls; fields; methods }) }

impls_spec:
  | IMPLS ids=id_ne_list { ids }
  | { [] }

id_ne_list:
  | id=IDENT { [id] }
  | id=IDENT COMMA rest=id_ne_list { id :: rest }

field_list:
  | /* empty */ { [] }
  | hd=field tl=field_list { hd :: tl }

field:
  | v=vdecl SEMI
    { (loc $startpos $endpos v )}

(* -----------------------
   arguments (function parameters)
   ----------------------- *)

arg:
  | i=IDENT t=ty_spec { (t, i) }

arg_list:
  | l=separated_list(COMMA, arg) { l }

(* -----------------------
   Types
   ----------------------- *)

ty:
  | ti=int_ty { TInt ti }
  | r=ref_ty { TRef r } 
  | tf=float_ty  { TFloat tf }
  | TBOOL   { TBool }
  | LPAREN t=ty RPAREN { t }
  // | t=ty OPT_TYPE { TOpt t }

int_ty:
  | Ti8 { TSigned Ti8 }
  | Ti16 { TSigned Ti16 }
  | Ti32 { TSigned Ti32 }
  | Ti64 { TSigned Ti64 }
  | Ti128 { TSigned Ti128 }
  | Tu8 { TUnsigned Tu8 }
  | Tu16 { TUnsigned Tu16 }
  | Tu32 { TUnsigned Tu32 }
  | Tu64 { TUnsigned Tu64 }
  | Tu128 { TUnsigned Tu128 }

float_ty:
  | Tf32 { Tf32 }
  | Tf64 { Tf64 }

ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

ret_ty_spec:
  | ARROW frtyp=ret_ty { frtyp }
  | { RetVoid }

ref_ty:
  | TSTRING { RString }
  | cname=IDENT { RClass cname }
  | LBRACKET t=ty SEMI sz=INT RBRACKET { RArray (t, sz) }
  | fun_ty=fun_ty { fun_ty }
  | gname=IDENT LT_TYPE params=separated_list(COMMA, ty) GT_TYPE
    { RGeneric (gname, params) }

fun_ty:
  | LBRACKET arg_tys=separated_list(COMMA, ty) RBRACKET THIN_ARROW rty=ret_ty 
    { RFun (arg_tys, rty) }

%inline bop:
  | PLUS  { Add }
  | MINUS { Sub }
  | MULT  { Mul }
  | DIV   { Div }
  | AT    { At }
  | MOD   { Mod }
  | POW   { Pow }
  | LT    { Lt }
  | LTE   { Lte }
  | LTLT  { Shl }
  | GTGT  { Lshr }
  | GTGTGT{ Ashr }
  | GT    { Gt }
  | GTE   { Gte }
  | NEQ   { Neq }
  | EQEQ  { Eqeq }
  | AND   { And }
  | OR    { Or }
  | XOR   { Xor }
  | CARET { BXor }
  | AMP   { BAnd }
  | BOR   { BOr }

%inline uop:
  | MINUS { Neg }
  | NOT   { Not }
  | TILDE { BNeg }

%inline aop:
  | EQUAL { Eq }
  | PLUEQ { PluEq }
  | MINEQ { MinEq }
  | TIMEQ { TimEq }
  | DIVEQ { DivEq }
  | ATEQ  { AtEq }
  | POWEQ { PowEq }
  | LTLTEQ { ShlEq }
  | GTGTEQ { LShrEq }
  | GTGTGTEQ { AShrEq }
  | CARETEQ { BXorEq }
  | AMPEQ { BAndEq }
  | BOREQ { BOrEq }

%inline ty_spec:
  | COLON t=ty { t }

(* -----------------------
   Expressions
   -----------------------
   structure:
     exp    -> left-recursive binary ops | unary
     unary  -> uop unary | postfix
     postfix-> primary { ( index | call | cast )* }
   This removes the ambiguity between calls/indexing and "reduce to primary vs shift to parse call".
   ----------------------- *)

exp:
  | e1=exp b=bop e2=exp
      { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=unary
      { u }

unary:
  | u=uop e=unary
      { loc $startpos $endpos @@ Uop (u, e) }
  | p=postfix
      { p }

(* primary forms (literals, ids, arrays, grouped) *)
primary:
  | TRUE                            { loc $startpos $endpos @@ Bool true }
  | FALSE                           { loc $startpos $endpos @@ Bool false }
  | NULL                            { loc $startpos $endpos @@ Null }
  | i=INT                           { loc $startpos $endpos @@ Int i }
  | f=FLOAT                         { loc $startpos $endpos @@ Float f }
  | s=STRING                        { loc $startpos $endpos @@ Str s }
  | id=IDENT                        { loc $startpos $endpos @@ Id id }
  | LBRACKET elems=separated_list(COMMA, exp) RBRACKET
      { loc $startpos $endpos @@ Array elems }
  | LPAREN e=exp RPAREN             { e }
  | NEW cid=IDENT LBRACE fields=separated_list(COMMA, field_init) RBRACE 
      { 
        let id_node = { elt = cid; loc = Range.mk_lex_range $startpos(cid) $endpos(cid) } in
        loc $startpos $endpos @@ ObjInit (id_node, fields)
      }

field_init:
  | fname=IDENT EQUAL e=exp
      {
        let fname_node = { elt = fname; loc = Range.mk_lex_range $startpos(fname) $endpos(fname) } in
        (fname_node, e)
      }

(* postfix: left-recursive so calls & indexes chain without ambiguity *)
postfix:
  | p=primary                                      { p }
  | p=postfix LBRACKET i=exp RBRACKET
      { loc $startpos $endpos @@ Index (p, i) }
  | p=postfix LPAREN args=separated_list(COMMA, exp) RPAREN
      { loc $startpos $endpos @@ Call (p, args) }
  // | cid=IDENT COLCOL fname=IDENT LPAREN args=separated_list(COMMA, exp) RPAREN
  //     { 
  //       let cid_node = { elt = cid; loc = Range.mk_lex_range $startpos(cid) $endpos(cid) } in
  //       let fname_node = { elt = fname; loc = Range.mk_lex_range $startpos(fname) $endpos(fname) } in
  //       loc $startpos $endpos @@ StaticCall (cid_node, fname_node, args)
  //     }
  | p=postfix AS t=ty
      { loc $startpos $endpos @@ Cast (p, t) }
  | p=postfix DOT i=IDENT
      { loc $startpos $endpos @@ Proj (p, i) }
  | l=lambda { l }

(* -----------------------
   LHS (for assignments)
   ----------------------- *)
lhs:
  | id=IDENT                        { loc $startpos $endpos @@ Id id }
  | p=postfix DOT i=IDENT 
      { loc $startpos $endpos @@ Proj (p, i) }
  | e=postfix LBRACKET i=exp RBRACKET
      { loc $startpos $endpos @@ Index (e, i) }

(* -----------------------
   variable decls / vdecl
   ----------------------- *)
vdecl:
  | LET id=IDENT t=ty_spec? EQUAL init=exp
      { (id, t, Some init, false) }
  | LET id=IDENT t=ty_spec?
      { (id, t, None, false) }
  | CONST id=IDENT t=ty_spec? EQUAL init=exp
      { (id, t, Some init, true) }


lambda: 
  | BAR args=untyped_lambda_args BAR THIN_ARROW stmts=block
      { loc $startpos $endpos @@ Lambda (args, stmts) }
  | LPAREN args=typed_lambda_args RPAREN THIN_ARROW rty=ret_ty stmts=block
      { loc $startpos $endpos @@ TypedLambda (args, rty, stmts) }

untyped_lambda_arg:
  | i=IDENT { i }

untyped_lambda_args:
  | l=separated_list(COMMA, untyped_lambda_arg) { l }

typed_lambda_arg:
  | i=IDENT t=ty_spec { (i, t) }

typed_lambda_args:
  | l=separated_list(COMMA, typed_lambda_arg) { l }

lambda_fun_ty_spec: 
  | COLON t=fun_ty  { t }

ldecl: 
  | FN id=IDENT t=lambda_fun_ty_spec? EQUAL l=lambda
      { 
        let i_start = $startpos(id) in 
        let i_end   = $endpos(id) in 
        let id_node = loc i_start i_end @@ id in
        (id_node, (match t with Some ty -> Some ty | None -> None), l) 
      }

(* -----------------------
   for-loop step (optional)
   ----------------------- *)
by_step:
  | BY step=exp { Some step }
  | { None }

(* -----------------------
   Statements
   ----------------------- *)

stmt:
  | d=vdecl SEMI
      { loc $startpos $endpos @@ Decl(d) }
  | l=ldecl SEMI
      { loc $startpos $endpos @@ LambdaDecl(l) }
  | p=lhs a=aop e=exp SEMI
      { loc $startpos $endpos @@ Assn(p,a,e) }
  | cs=call_stmt
      { cs }
  | ifs=if_stmt
      { ifs }
  | RETURN e=exp? SEMI
      { loc $startpos $endpos @@ Ret(e) }
  | WHILE e=exp b=block
      { loc $startpos $endpos @@ While(e, b) }
  | FOR iter=iterator IN iterable=exp b=block
      { loc $startpos $endpos @@ ForEach(iter, iterable, b) }
  | FOR iter=iterator IN over=range step=by_step b=block 
      { loc $startpos $endpos @@ For(iter, over, step, b) }
  | CONT SEMI
      { loc $startpos $endpos @@ Continue }
  | BREAK SEMI
      { loc $startpos $endpos @@ Break }
  | DEL es=separated_list(COMMA, exp) SEMI
      { loc $startpos $endpos @@ Del(es) }

range: 
  | start=exp RANGE fin=exp
    { (start, fin, false) }
  | start=exp RANGE_INCL fin=exp 
    { (start, fin, true) }


(* call as a statement; we parse it separately to produce an SCall node.
   Because Calls are handled by postfix, we accept: postfix LPAREN args RPAREN SEMI  *)
call_stmt:
  | p=postfix LPAREN args=separated_list(COMMA, exp) RPAREN SEMI
      { loc $startpos $endpos @@ SCall (p, args) }

iterator:
  | i=IDENT { loc $startpos $endpos @@ i }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF e=exp b1=block b2=else_stmt
      { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | /* empty */       { [] }          (* no else -> empty block *)
  | ELSE b=block      { b }           (* else { ... } *)
  | ELSE ifs=if_stmt  { [ ifs ] }     (* else if ... -> else block with one stmt *)

