(* Coral.Type

   Represents a type of a coral expression.

 *)
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

type 'expr t =
  | Name of string
  | Parameterized of ('expr t * 'expr t list)
  | Dotted of 'expr t * string
  | Free of int * string
  | Decl of { metatype : string }
[@@deriving show, sexp_of, compare]
