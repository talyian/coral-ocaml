(* Coral.Type Represents a type of a coral expression. *)
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

type 'expr t =
  | Name of string
      (** a type name such as "Int" present in source code, will be resolved during name resolution *)
  | Parameterized of ('expr t * 'expr t list)
  | Dotted of 'expr t * string  (** A pseudotype used for declaring varargs *)
  | Free of int * string  (** a type variable used for polymorphism *)
  | Decl of {metatype: string}
[@@deriving show, sexp_of, compare]
