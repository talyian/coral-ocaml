open Ppx_sexp_conv_lib.Conv
open Ppx_compare_lib.Builtin

type t = int [@@deriving show, sexp_of, compare]

let next : unit -> t =
  let counter = ref 100 in
  fun () ->
    counter := !counter + 1;
    !counter
