(* A scope is a lexical scope that contains a mapping of string names to nodes let x = 1 loop: let
   y = "2" <-- here the scope would be {"y" -> Let(y, 2), parents=[{"x" -> Let(x, 1)}]} *)

open Base

module Scope = struct
  type t = {
    parent : t list;
    names : (String.t, Ast.Node.t, String.comparator_witness) Base.Map.t;
  }

  let empty = { parent = []; names = Map.empty (module String) }

  let nest parent = { empty with parent = [ parent ] }

  let add key data scope = { scope with names = Map.add_exn ~key ~data scope.names }

  let rec find ~name scope =
    match Map.find scope.names name with
    | Some v -> Some v
    | None -> List.find_map ~f:(find ~name) scope.parent
end

(* Names.t names a nameresolver with a mapping of refs from Vars to Exprs that they reference *)
module Names = struct
  type t = { current_scope : Scope.t; refs : Ast.Node.t Ast.Node.Map.t }

  let empty = { current_scope = Scope.empty; refs = Map.empty (module Ast.Node) }

  let deref t node = Map.find t.refs node
end

include Names
