open Coral_core
open Base

module Scope = struct
  type t = {
    parent : t list;
    names : (String.t, Ast.node, String.comparator_witness) Base.Map.t;
  }

  let empty = { parent = []; names = Map.empty (module String) }

  let nest parent = { empty with parent = [ parent ] }

  let add key data scope =
    { scope with names = Map.add_exn ~key ~data scope.names }

  let rec find ~name scope =
    match Map.find scope.names name with
    | Some v -> Some v
    | None -> List.find_map ~f:(find ~name) scope.parent
end

module Names = struct
  type t = {
    current_scope : Scope.t;
    refs : (Ast.node, Ast.node, Ast.Node.comparator_witness) Base.Map.t;
  }

  let empty =
    { current_scope = Scope.empty; refs = Map.empty (module Ast.Node) }
end

let rec run (data : Names.t) e =
  match e with
  | Ast.Var { name; _ } -> (
      match Scope.find ~name data.current_scope with
      | None ->
          Stdio.printf "name not found: %s\n" name;
          data
      | Some expr ->
          { data with refs = Map.add_exn ~key:e ~data:expr data.refs } )
  | Ast.Module { name = _; lines } -> List.fold ~init:data ~f:run lines
  | Ast.Tuple lines | Ast.List lines | Ast.Block lines ->
      List.fold ~init:data ~f:run lines
  | Ast.Func { name; params; body; _ } ->
      let outer_scope = Scope.add name e data.current_scope in
      let inner_scope = Scope.nest data.current_scope in
      let data = { data with current_scope = inner_scope } in
      let data = List.fold params ~init:data ~f:run in
      let data = run data body in
      { data with current_scope = outer_scope }
  | Ast.Let (var, value) ->
      let data = run data value in
      let scope = Scope.add var.name e data.current_scope in
      { data with current_scope = scope }
  | Ast.Call { callee; args } ->
      let data = run data callee in
      let data = List.fold args ~init:data ~f:run in
      data
  | Ast.FloatLiteral _ | Ast.IntLiteral _ | Ast.StringLiteral _ -> data
  | Ast.Return v -> run data v
  | _ ->
      Stdio.print_endline @@ Ast.nodeName e;
      data

let resolve e = run Names.empty e

let show n =
  Map.iteri n.Names.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show_node key) (Ast.nodeName data))
