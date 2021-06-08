open Base

module type Data = sig
  type t [@@deriving sexp_of]

  val string_of_t : t -> string
end

module Make (Data : Data) = struct
  type term = int [@@deriving compare, sexp_of]

  type expr =
    | ConstInt of int64
    | Term of term
    | StructDef of {name: string; fields: (string * term) list}
  [@@deriving compare, sexp_of]

  type t = {debug: bool; x: int}

  let empty : t = {debug= false; x= 0}
  let add_term foo t = (t, 0)
  let const_term_for expr t = (t, 0)
  let term_is term (expr : expr) t = t
end
