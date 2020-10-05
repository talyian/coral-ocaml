(* Coral.Type

   Represents a type of a coral expression.

 *)
open Sexplib0.Sexp_conv

type 'expr t =
  | Name of string
  | Parameterized of ('expr t * 'expr t list)
  | Dotted of 'expr t * string
  | Free of int * string
  | Decl of { metatype : string }
[@@deriving show, sexp_of]
