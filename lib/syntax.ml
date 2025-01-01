type expr =
  | Decimal of Decimal.t
  | Negate of expr
  | Add of (expr * expr)
  | Subtract of (expr * expr)
  | Multiply of (expr * expr)
  | Divide of (expr * expr)
  | Function of (string list * expr)
  | Call of (expr * expr list)
  | Var of string

type date = { year : int; month : int; day : int } [@@deriving make]
type posting = { account : string; amount : expr option } [@@deriving make]

type decl =
  | Tx of {
      date : date;
      desc : string;
      tags : string list;
      postings : posting list option;
    }

type program = { decls : decl list }
