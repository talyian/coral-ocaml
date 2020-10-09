open Coral_core
open Base

(* A scope is a lexical scope that contains a mapping of string names to nodes
let x = 1
loop:
   let y = "2"
   <-- here the scope would be {"y" -> Let(y, 2), parents=[{"x" -> Let(x, 1)}]}
 *)

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

(* Names.t names a nameresolver with a mapping of refs from Vars to Exprs that they reference *)
module Names = struct
  type t = { current_scope : Scope.t; refs : Ast.Node.t Ast.Node.Map.t }

  let empty =
    { current_scope = Scope.empty; refs = Map.empty (module Ast.Node) }

  let deref (data : t) node = Map.find data.refs node
end

let rec run (data : Names.t) node : Names.t =
  let e, _ = node in
  match e with
  | Ast.Var { name; _ } ->
      let reference = Scope.find ~name data.current_scope in
      let reference = Option.value_exn ~message:name reference in
      { data with refs = Map.add_exn data.refs ~key:node ~data:reference }
  | Ast.Extern { name; _ } ->
      let scope = Scope.add name node data.current_scope in
      { data with current_scope = scope }
  | Ast.Func { name; params; body; _ } ->
      let outer_scope = Scope.add name node data.current_scope in
      let inner_scope = Scope.nest data.current_scope in
      let data = { data with current_scope = inner_scope } in
      let data = List.fold params ~init:data ~f:run in
      let data = run data body in
      { data with current_scope = outer_scope }
  | Ast.Let (var, value) ->
      let data = run data value in
      let scope = Scope.add var.name node data.current_scope in
      { data with current_scope = scope }
  | _ -> Ast.fold ~init:data ~f:run node

let show n =
  Map.iteri n.Names.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show_node key)
        (Ast.nodeName (fst data)))

let resolve e =
  let global_scope =
    let open Ast in
    let open Builtins in
    Scope.empty
    |> Scope.add "+" (mm @@ Builtin ADD)
    |> Scope.add "-" (mm @@ Builtin SUB)
    |> Scope.add "=" (mm @@ Builtin EQ)
  in
  let x = run { Names.empty with current_scope = global_scope } e in
  x
