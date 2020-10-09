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
(* intrinsics *)
[@@deriving show, sexp_of, compare]
