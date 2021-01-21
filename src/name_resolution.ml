open Coral_core
open Base

module Scope = struct
  type t = Scope of {parents: t list; names: Ast.t Map.M(String).t}

  let names = function Scope {names; _} -> names
  let parents = function Scope {parents; _} -> parents

  let rec find ~name t : Ast.t option =
    match Map.find (names t) name with
    | Some value -> Some value
    | None -> List.find_map (parents t) ~f:(find ~name)

  let add name node (Scope t) = Scope {t with names= Map.add_exn t.names ~key:name ~data:node}
  let create () = Scope {parents= []; names= Map.empty (module String)}

  let nest t =
    let (Scope x) = create () in
    Scope {x with parents= [t]}
end

module NameTraversal = struct
  (* data for traversing an AST and building doing name resolution *)
  type t =
    { refs: Ast.Node.t Map.M(Ast.Node).t (* a map from var nodes to its reference *)
    ; returns: Ast.Node.t Map.M(Ast.Node).t
          (* A map from a return to the function it returns from *)
    ; current_scope: Scope.t }

  let empty () =
    { refs= Map.empty (module Ast.Node)
    ; returns= Map.empty (module Ast.Node)
    ; current_scope= Scope.create () }
end

type t = NameTraversal.t

(* Builds a Names.t by traversing an Ast *)
let rec run imports (data : NameTraversal.t) (node : Ast.t) : NameTraversal.t =
  let run = run imports in
  match !node with
  | Ast.Var {name; _} ->
      let reference = Scope.find ~name data.current_scope in
      let reference = Option.value_exn ~message:("Name not found: " ^ name) reference in
      {data with refs= Map.set data.refs ~key:node ~data:reference}
  | Extern {binding= _; name; typ; info} ->
      let data = run data typ in
      let scope = Scope.add name node data.current_scope in
      {data with current_scope= scope}
  | Func {name; params; body; ret_type; info} ->
      let outer_scope = Scope.add name node data.current_scope in
      let inner_scope = Scope.nest outer_scope in
      let inner_scope = Scope.add "__function__" node inner_scope in
      let data = Option.fold ~f:run ~init:data ret_type in
      let data = {data with current_scope= inner_scope} in
      let data = List.fold params ~init:data ~f:run in
      let data = run data body in
      {data with current_scope= outer_scope}
  | Param {name; typ; idx= _; info} ->
      let data = Option.fold ~f:run ~init:data typ in
      {data with current_scope= Scope.add name node data.current_scope}
  | Return {value; info} ->
      let data = run data value in
      let func = Scope.find ~name:"__function__" data.current_scope in
      let func = Option.value_exn ~message:"function not found for return" func in
      {data with returns= Map.set data.returns ~key:node ~data:func}
  | Let {name; typ; value; info} ->
      let outer_scope = data.current_scope in
      let data = Option.fold ~f:run ~init:data typ in
      let data = run {data with current_scope= Scope.nest data.current_scope} value in
      let data = {data with current_scope= outer_scope} in
      let scope = Scope.add name node data.current_scope in
      {data with current_scope= scope}
  | _ -> Ast.fold_info ~init:data ~f:run node

let show n =
  Stdio.printf "Names\n" ;
  Map.iteri n.NameTraversal.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show key) (Ast.show data))

let default_global_scope = Builtin_defs.initialize_names ~init:(Scope.create ()) ~f:Scope.add

let resolve imports =
  let x = NameTraversal.empty () in
  run imports
    {x with current_scope= default_global_scope}
    imports.Coral_frontend.Import_resolution.Imports.main
