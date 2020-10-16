type t =
  (* Operators *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | EQ
  | LT
  | GT
  | LTE
  | GTE
  | NEQ
  | AND
  | OR
  (* primitive types *)
  | VOID
  | BOOL
  | INT8
  | INT16
  | INT32
  | INT64
  | INTNATIVE
  | FLOAT32
  | FLOAT64
  | STR
  | PTR
  | FUNC
  | VARFUNC
(* intrinsics *)
[@@deriving show, sexp_of, compare, equal]
