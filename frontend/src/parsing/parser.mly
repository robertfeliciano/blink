%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}


/* tokens */
%token EOF
%token <Z.t>  INT
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
%token BY       /* by */
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
%left RANGE RANGE_INCL
%nonassoc LOW
%nonassoc QMARK
%nonassoc NOT
%nonassoc LBRACKET
%nonassoc LPAREN

%start program
%start exp_top
%start stmt_top

%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.program> program
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp EOF { e }

stmt_top:
  | s=stmt EOF { s }

program: 
  | fns=list(fdecl) EOF { Prog(fns) }

fdecl:
  | FUN fname=IDENT LPAREN args=arglist RPAREN frtyp=ret_ty_spec body=block
    { (loc $startpos $endpos { frtyp; fname; args; body }) }

%inline arg:
  | i=IDENT t=ty_spec { (t, i) }

%inline arglist:
  | l=separated_list(COMMA, arg) { l }

ty:
  | ti=int_ty { TInt ti }
  | r=ref_ty { TRef r } %prec LOW
  | tf=float_ty  { TFloat tf }
  | TBOOL   { TBool }
  | LPAREN t=ty RPAREN { t }

%inline int_ty: 
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

%inline float_ty: 
  | Tf32 { Tf32 }
  | Tf64 { Tf64 }


%inline ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

%inline ret_ty_spec:
  | ARROW frtyp=ret_ty { frtyp }

%inline ref_ty:
  | TSTRING { RString }
  | LBRACKET t=ty SEMI sz=INT RBRACKET { RArray (t, sz) }
  | tl=ty RANGE tr=ty { RRange (tl, tr) }

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

lhs:  
  | id=IDENT                        { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                                    { loc $startpos $endpos @@ Index (e, i) }
  // | e=exp DOT id=IDENT  { loc $startpos $endpos @@ Proj (e, id) }

%inline array:
  | LBRACKET rows=separated_list(COMMA, array_row) RBRACKET
    { loc $startpos $endpos @@ Array rows }

%inline array_row:
  | elems=separated_list(COMMA, exp) { elems }  (* A single row is a list of expressions *)

exp:
  | TRUE                            { loc $startpos $endpos @@ Bool true }
  | FALSE                           { loc $startpos $endpos @@ Bool false }
  | i=INT                           { loc $startpos $endpos @@ Int i }
  | f=FLOAT                         { loc $startpos $endpos @@ Float f }
  | s=STRING                        { loc $startpos $endpos @@ Str s }
  | id=IDENT                        { loc $startpos $endpos @@ Id id }
  // | arr=array                       { loc $startpos $endpos @@ arr }
  | LBRACKET elems=separated_list(COMMA, exp) RBRACKET
                                    { loc $startpos $endpos @@ Array elems }
  | e=exp LBRACKET i=exp RBRACKET   { loc $startpos $endpos @@ Index (e, i) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN
                                    { loc $startpos $endpos @@ Call (e, es) }
  | e1=exp b=bop e2=exp             { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=uop e=exp                     { loc $startpos $endpos @@ Uop (u, e) }
  | e1=exp RANGE e2=exp             { loc $startpos $endpos @@ Range(e1, e2, false) }
  | e1=exp RANGE_INCL e2=exp        { loc $startpos $endpos @@ Range(e1, e2, true) }
  | LPAREN e=exp RPAREN             { e }

vdecl:
  | LET id=IDENT t=ty_spec? EQUAL init=exp { (id, t, init, false) }
  | CONST id=IDENT t=ty_spec? EQUAL init=exp { (id, t, init, true) }


%inline
by_step:
  | BY step=exp { Some step }
  | { None }

stmt:
  | d=vdecl SEMI                    { loc $startpos $endpos @@ Decl(d) }
  | p=lhs a=aop e=exp SEMI          { loc $startpos $endpos @@ Assn(p,a,e) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN SEMI 
                                    { loc $startpos $endpos @@ SCall (e, es) }
  | ifs=if_stmt                     { ifs }
  | RETURN e=exp? SEMI              { loc $startpos $endpos @@ Ret(e) }
  | WHILE e=exp b=block             { loc $startpos $endpos @@ While(e, b) } 
  | FOR iter=iterator IN iterable=exp step=by_step b=block
                                    { loc $startpos $endpos @@ For(iter, iterable, step, b) }
                                    // { loc $startpos $endpos @@ For(iter, e, step, b)}
  | CONT SEMI                       { loc $startpos $endpos @@ Continue }
  | BREAK SEMI                      { loc $startpos $endpos @@ Break }


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