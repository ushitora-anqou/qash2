%{
%}

%token APP
%token BR
%token DEDENT
%token EOF
%token FUN
%token IMPORT
%token INDENT
%token LPAREN
%token MINUS
%token MOD
%token PLUS
%token PROC
%token RARROW
%token RPAREN
%token SLASH
%token STAR

%token <Syntax.date> DATE
%token <Decimal.t>   DECIMAL
%token <string>      ID
%token <int>         SPACE
%token <string>      STRING
%token <string>      TAG

(* cf. https://ptival.github.io/2017/05/16/parser-generators-and-function-application/ *)
%nonassoc EXPR_STMT RARROW
%left MINUS PLUS
%left MOD SLASH STAR
%nonassoc DECIMAL ID LPAREN
%nonassoc APP

%start program
%type <Syntax.program> program

%start expr
%type <Syntax.expr> expr
%%

expr :
| e=Expr EOF {
  e
}

program :
| decls=Decls {
  Syntax.{ decls }
}

Decls :
| BR* x=Decl xs=Decls {
  x :: xs
}
| BR* EOF {
  []
}

Decl :
| x=Transaction {
  Syntax.Transaction x
}
| IMPORT format=ioption(STRING) path=STRING
  overlays=ioption(INDENT overlays=Transactions DEDENT { overlays }) {
  Syntax.Import { format; path; overlays = Option.value ~default:[] overlays }
}
| PROC name=ID params=nonempty_list(ID) INDENT stmts=Stmts DEDENT {
  Syntax.Proc { name; params; stmts }
}
| s=Stmt {
  Syntax.Stmt s
}

Stmts :
| BR* x=Stmt xs=ioption(Stmts) {
  x :: (Option.value ~default:[] xs)
}

Stmt :
| e=AtomicExpr %prec EXPR_STMT {
  Syntax.Expr e
}

Transaction :
| STAR date=DATE desc=STRING tags=list(TAG)
  postings=option(INDENT ps=separated_list(BR, Posting) DEDENT { ps }) {
  Syntax.{ date; desc; tags; postings }
}

Transactions :
| BR* x=Transaction xs=ioption(Transactions) {
  x :: (Option.value ~default:[] xs)
}

Posting :
| account=ID amount=option(Expr) {
  Syntax.make_posting ~account ?amount ()
}

Expr :
| MINUS e=Expr {
  Syntax.Negate e
}
| e1=Expr PLUS e2=Expr {
  Syntax.Add (e1, e2)
}
| e1=Expr MINUS e2=Expr {
  Syntax.Subtract (e1, e2)
}
| e1=Expr STAR e2=Expr {
  Syntax.Multiply (e1, e2)
}
| e1=Expr SLASH e2=Expr {
  Syntax.Divide (e1, e2)
}
| e1=Expr MOD e2=Expr {
  Syntax.Modulo (e1, e2)
}
| FUN params=nonempty_list(ID) RARROW e=Expr {
  Syntax.Function (params, e)
}
| e=AtomicExpr {
  e
}

AtomicExpr :
| d=DECIMAL {
  Syntax.Decimal d
}
| x=ID {
  Syntax.Var x
}
| e1=AtomicExpr e2=AtomicExpr %prec APP {
  Syntax.Apply (e1, e2)
}
| LPAREN e=Expr RPAREN {
  e
}
