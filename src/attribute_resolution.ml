open Coral_core

(* Converts an ast with attributes into an ast without attributes but with tags *)
(* requires name resolution *)
module Attribute = struct
  type t = Overload
  type 'a v = Overload : unit -> unit v

  module Name = struct
    type attr = t
    type t = Overload

    let of_attribute : attr -> t = function Overload -> Overload
  end
end

module type S = sig
  type t

  val get_attributes : t -> Ast.Node.t -> Attribute.t list
  val read_attribute : t -> Ast.Node.t -> Attribute.Name.t -> Attribute.t option
end

(* let rec convert_attributes s = function
 *   | {contents= Ast.Node.Decorated {target; _}} ->
 *       Stdio.print_s [%sexp "convert_attributes: found attribute"] ;
 *       (s, target)
 *   | node -> Coral_core.Ast.fold_map ~init:s ~f:convert_attributes node *)

(* let rec convert_attributes init expr =
 *   match !expr with
 *   | Ast.Decorated {target; _} -> (init, target)
 *   | node -> (Ast.fold ~init ~f:(convert_attributes node, node) *)

(* let run ?init e =
 *   let init = match init with Some init -> init | None -> () in
 *   convert_attributes init e *)
