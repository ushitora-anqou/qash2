%{
%}

%token BR EOF COMMA INDENT DEDENT PLUS MINUS PIPE STAR SLASH K_MOD K_IMPORT K_PROC

%token <int> SPACE
%token <Decimal.t> DECIMAL
%token <(int * int * int)> DATE
%token <string> ID
%token <string> STRING
%token <string> TAG

%start toplevel
%type <unit> toplevel
%%

toplevel :
| EOF {
  ()
}

(*
Toplevel :
| BR* x=Directive xs=Toplevel {
  x :: xs
}
| BR* EOF {
  []
}

Date :
| year=DECIMAL MINUS month=DECIMAL MINUS day=DECIMAL {
  Model.{ year; month; day }
}

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
