open Coral_core
open Base

module Scope = struct
  type t = {
    parent : t list;
    names : (String.t, Ast.Node.t, String.comparator_witness) Base.Map.t;
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
  type t = { current_scope : Scope.t; refs : Ast.Node.t Ast.Node.Map.t }

  let empty =
    { current_scope = Scope.empty; refs = Map.empty (module Ast.Node) }
end

let rec run (data : Names.t) node =
  let e, _ = node in
  match e with
  | Ast.Var { name; _ } -> (
      match Scope.find ~name data.current_scope with
      | None ->
          Stdio.printf "name not found: %s\n" name;
          data
      | Some expr ->
          Stdio.printf "found name: %s\n" name;
          { data with refs = Map.add_exn ~key:node ~data:expr data.refs } )
  | Ast.Module { name = _; lines } -> List.fold ~init:data ~f:run lines
  | Ast.Tuple lines | Ast.List lines | Ast.Block lines ->
      List.fold ~init:data ~f:run lines
  | Ast.Func { name; params; body; _ } ->
      let outer_scope = Scope.add name node data.current_scope in
      let inner_scope = Scope.nest data.current_scope in
      let data = { data with current_scope = inner_scope } in
      let data = List.fold params ~init:data ~f:run in
      let data = run data body in
      { data with current_scope = outer_scope }
  | Ast.Extern { name; _ } ->
      let scope = Scope.add name node data.current_scope in
      { data with current_scope = scope }
  | Ast.Let (var, value) ->
      let data = run data value in
      let scope = Scope.add var.name node data.current_scope in
      { data with current_scope = scope }
  | Ast.Call { callee; args } ->
      let data = run data callee in
      let data = List.fold args ~init:data ~f:run in
      data
  | Ast.FloatLiteral _ | Ast.IntLiteral _ | Ast.StringLiteral _ -> data
  | Ast.Return v -> run data v
  | _ ->
      Stdio.printf "unknown name handle %s\n" @@ Ast.nodeName e;
      data

let show n =
  Map.iteri n.Names.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show_node key)
        (Ast.nodeName (fst data)))

let resolve e =
  let x = run Names.empty e in
  x
