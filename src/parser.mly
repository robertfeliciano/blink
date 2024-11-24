%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}


/* tokens */
%token EOF
%token <int64>  INT
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
%token DOTDOT    /* .. */
%token COLON     /* : */
%token SEMI      /* ; */
%token EQUAL     /* = */
%token ARROW     /* => */
%token PLUS      /* + */
%token MINUS     /* - */
%token MULT      /* * */
%token DIV       /* / */
%token AT        /* @ */
%token MOD       /* % */
%token POW       /* ** */
%token PLUEQ     /* += */
%token MINEQ     /* -= */
%token TIMEQ     /* *= */
%token DIVEQ     /* /= */
%token ATEQ      /* @= */
%token POWEQ     /* **= */
%token LT        /* < */
%token LTE       /* <= */
%token GT        /* > */
%token GTE       /* >= */
%token NEQ       /* != */
%token EQEQ      /* == */
%token AND       /* and */
%token OR        /* or */
%token NOT       /* not */
%token LET       /* let */
%token NEW       /* new */
%token CONST     /* const */
%token TVOID     /* void */
%token TINT      /* int */
%token TFLOAT    /* float */
%token TSTRING   /* string */
%token TBOOL     /* bool */
%token FUN       /* fun */
%token IF        /* if */
%token IN        /* in */
%token ELSE      /* else */
%token FOR       /* for */
%token WHILE     /* while */
%token BREAK     /* break */
%token CONT      /* continue */
%token RETURN    /* return */
%token TRUE      /* true */
%token FALSE     /* false */
%token WHERE     /* where */
%token IMPORT    /* import */
%token ENABLE    /* enable */
%token CLASS     /* class */
%token INHERITS  /* inherits */
%token GLOBAL    /* global */
%token QMARK     /* ? */

%right POW
%right EQUAL PLUEQ MINEQ TIMEQ DIVEQ ATEQ POWEQ
%left OR
%left AND
%left EQEQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left MULT DIV MOD  
%left AT
%left DOT
%nonassoc LOW
%nonassoc QMARK
%nonassoc NOT
%nonassoc LBRACKET
%nonassoc LPAREN

%start prog
%start exp_top
%start stmt_top

%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.prog> prog
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp EOF { e }

stmt_top:
  | s=stmt EOF { s }

prog:
  | p=list(decl) EOF  { p }

decl:
  // | GLOBAL name=IDENT EQ init=gexp SEMI
  //   { Gvdecl (loc $startpos $endpos { name; init }) }
  | FUN fname=IDENT LPAREN args=arglist RPAREN ARROW frtyp=ret_ty body=block
    { Gfdecl (loc $startpos $endpos { frtyp; fname; args; body }) }
//   | STRUCT name=UIDENT LBRACE fs=separated_list(SEMI, decl_field) RBRACE 
//     { Gtdecl (loc $startpos $endpos (name, fs)) }

arg:
  | i=IDENT COLON t=ty { (t, i) }

arglist:
  | l=separated_list(COMMA, arg) { l }

ty:
  | TINT    { TInt }
  | r=ref_ty { TRef r } %prec LOW
  | TFLOAT  { TFloat }
  | TBOOL   { TBool }
  | LPAREN t=ty RPAREN { t }

%inline ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

%inline ref_ty:
  | TSTRING { RString }
  | LBRACKET t=ty RBRACKET { RArray t }

%inline bop:
  | PLUS  { Add }
  | MINUS { Sub }
  | MULT  { Mul }
  | DIV   { Div }
  | AT    { Dot }
  | MOD   { Mod }
  | POW   { Pow }
  | LT    { Lt }
  | LTE   { Lte }
  | GT    { Gt }
  | GTE   { Gte }
  | NEQ   { Neq }
  | EQEQ  { Eqeq }
  | AND   { And }
  | OR    { Or }

%inline uop: 
  | MINUS { Neg }
  | NOT   { Not }

%inline aop:
  | EQUAL { Eq }
  | PLUEQ { PluEq }
  | MINEQ { MinEq }
  | TIMEQ { TimEq }
  | DIVEQ { DivEq }
  | ATEQ  { AtEq }
  | POWEQ { PowEq }

%inline ty_spec:
  | COLON t=ty { t }

%inline range:
  // | l=exp DOTDOT r=exp { (l, r, false) }
  | l=exp DOTDOT e=EQUAL? r=exp { loc $startpos $endpos @@ Range(l, r, e <> None ) }

lhs:  
  | id=IDENT                        { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                                    { loc $startpos $endpos @@ Index (e, i) }
  // | e=exp DOT id=IDENT  { loc $startpos $endpos @@ Proj (e, id) }

// %inline array:
//   | LBRACKET rows=separated_list(SEMI, array_row) RBRACKET
//     { loc $startpos $endpos @@ Array (List.map (fun row -> Array row) rows) }

// %inline array_row:
//   | LBRACKET elems=list(exp) RBRACKET
//     { elems }  (* A single row is a list of expressions *)

exp:
  | TRUE                            { loc $startpos $endpos @@ Bool true }
  | FALSE                           { loc $startpos $endpos @@ Bool false }
  | i=INT                           { loc $startpos $endpos @@ Int i }
  | f=FLOAT                         { loc $startpos $endpos @@ Float f }
  | s=STRING                        { loc $startpos $endpos @@ Str s }
  | id=IDENT                        { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET   { loc $startpos $endpos @@ Index (e, i) }
  // | arr=array                       { loc $startpos $endpos @@ arr }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN
                                    { loc $startpos $endpos @@ Call (e, es) }
  | e1=exp b=bop e2=exp             { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=uop e=exp                     { loc $startpos $endpos @@ Uop (u, e) }
  | LPAREN e=exp RPAREN             { e }

vdecl:
  | LET id=IDENT t=ty_spec? EQUAL init=exp { (id, t, init, false) }
  | CONST id=IDENT t=ty_spec? EQUAL init=exp { (id, t, init, true) }

stmt:
  | d=vdecl SEMI                    { loc $startpos $endpos @@ Decl(d) }
  | p=lhs a=aop e=exp SEMI          { loc $startpos $endpos @@ Assn(p,a,e) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN SEMI 
                                    { loc $startpos $endpos @@ SCall (e, es) }
  | ifs=if_stmt                     { ifs }
  | RETURN e=exp? SEMI              { loc $startpos $endpos @@ Ret(e) }
  | WHILE e=exp b=block             { loc $startpos $endpos @@ While(e, b) } 
  | FOR iter=iterator IN r=range b=block 
                                    { loc $startpos $endpos @@ For(iter, r, b)}
  | CONT                            { loc $startpos $endpos @@ Continue }
  | BREAK                           { loc $startpos $endpos @@ Break }

%inline iterator:
  | i=IDENT { loc $startpos $endpos @@ i }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF e=exp b1=block b2=else_stmt
    { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }