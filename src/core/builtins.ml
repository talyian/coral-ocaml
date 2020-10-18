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
  (* Integer primitives *)
  | ADD_INT
  | SUB_INT
  | MUL_INT
  | DIV_INT
  | MOD_INT
  | EQ_INT
  | LT_INT
  | GT_INT
  | LTE_INT
  | NEQ_INT
  | GTE_INT
  (* float primitives *)
  | ADD_FLOAT
  | SUB_FLOAT
  | MUL_FLOAT
  | DIV_FLOAT
  | MOD_FLOAT
  | EQ_FLOAT
  | LT_FLOAT
  | GT_FLOAT
  | LTE_FLOAT
  | NEQ_FLOAT
  | GTE_FLOAT
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
[@@deriving show { with_path = false }, sexp_of, compare, equal]
