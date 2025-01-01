%{
%}

%token BR
%token COMMA
%token DEDENT
%token EOF
%token INDENT
%token K_IMPORT
%token K_MOD
%token K_PROC
%token LPAREN
%token MINUS
%token PIPE
%token PLUS
%token RPAREN
%token SLASH
%token STAR

%token <Syntax.date> DATE
%token <Decimal.t>   DECIMAL
%token <string>      ID
%token <int>         SPACE
%token <string>      STRING
%token <string>      TAG

%left PIPE
%left PLUS MINUS
%left STAR SLASH
%left LPAREN

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
| x=Tx {
  x
}

Tx :
| STAR date=DATE desc=STRING tags=list(TAG)
  postings=option(INDENT ps=separated_list(BR, Posting) DEDENT { ps }) {
  Syntax.Tx { date; desc; tags; postings }
}

Posting :
| account=ID amount=option(Expr) {
  Syntax.make_posting ~account ?amount ()
}

Expr :
| x=ID {
  Syntax.Var x
}
| d=DECIMAL {
  Syntax.Decimal d
}
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
| PIPE params=separated_list(COMMA, ID) PIPE e=Expr {
  Syntax.Function (params, e)
}
| e=Expr LPAREN args=separated_list(COMMA, Expr) RPAREN {
  Syntax.Call (e, args)
}
| LPAREN e=Expr RPAREN {
  e
}

(*
Directive :
(* !open-account *)
| K_OPEN_ACCOUNT kind=ID account=Account currency=ID tags=list(TAG) {
  Model.OpenAccount {
    account;
    currency;
    kind =
      (
        match kind with
        | "asset" -> Model.Asset
        | "liability" -> Liability
        | "equity" -> Equity
        | "income" -> Income
        | "expense" -> Expense
        | _ -> failwith "invalid account kind"
      );
    tags;
  }
}
| K_IMPORT filename=STRING {
  Model.Import {
    filename;
    transactions = [];
  }
}
| K_IMPORT filename=STRING INDENT transactions=Transactions DEDENT {
  Model.Import {
    filename;
    transactions;
  }
}
| K_ASSERT sql=STRING {
  Model.Assert sql
}
| K_SHOW sql=STRING {
  Model.Show sql
}
(* Transaction *)
| x=Transaction {
  Model.Transaction x
}

Transactions :
| BR* x=Transaction xs=Transactions {
  x :: xs
}
| BR* x=Transaction {
  [x]
}

Transaction :
| STAR date=Date narration=STRING tags=list(TAG)
  postings=option(INDENT ps=separated_list(BR, Posting) DEDENT { ps }) {
  let postings = Option.value ~default:[] postings in
  Model.make_transaction ~date ~narration ~postings ~tags ()
}

Posting :
| account=Account amount=option(ArithExpr) {
  Model.make_posting ~account ?amount ()
}

ArithExpr :
| i=DECIMAL {
  i
}
| MINUS i=DECIMAL {
  -i
}

Account :
| id=ID {
  String.split_on_char ':' id
}
*)
