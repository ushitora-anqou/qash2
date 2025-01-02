type expr =
  | Decimal of Decimal.t
  | Negate of expr
  | Add of (expr * expr)
  | Subtract of (expr * expr)
  | Multiply of (expr * expr)
  | Divide of (expr * expr)
  | Modulo of (expr * expr)
  | Function of (string list * expr)
  | Apply of (expr * expr)
  | Var of string

type date = { year : int; month : int; day : int } [@@deriving make]
type posting = { account : string; amount : expr option } [@@deriving make]

type transaction = {
  date : date;
  desc : string;
  tags : string list;
  postings : posting list option;
}

type stmt = Expr of expr

type decl =
  | Transaction of transaction
  | Import of {
      format : string option;
      path : string;
      overlays : transaction list;
    }
  | Proc of { name : string; params : string list; stmts : stmt list }

type program = { decls : decl list }
