type expr =
  | Int of int
  | Eof
  | Atom of string
  | Float of float
  | ConsCell of expr * expr
  | List of expr list
  | String of string
  | Defun of string * string list * expr
  | Def of string * expr
  | Quoted of expr
  | Fn of string list * expr
[@@deriving show { with_path = false }, eq, ord]
