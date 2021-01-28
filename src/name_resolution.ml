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
    { refs: Ast.Node.t Map.M(Ast.Node).t  (** a map from var nodes to its reference *)
    ; returns: Ast.Node.t Map.M(Ast.Node).t
          (** A map from a return to the function it returns from *)
    ; members: Ast.Node.t Map.M(Names.Member).t
    ; current_scope: Scope.t
    ; global_scope: Scope.t  (** we start from this every time we parse a module *) }

  let empty () =
    { refs= Map.empty (module Ast.Node)
    ; returns= Map.empty (module Ast.Node)
    ; members= Map.empty (module Names.Member)
    ; current_scope= Scope.create ()
    ; global_scope= Scope.create () }

  let merge data imported_names =
    { data with
      refs= Map.merge_skewed data.refs imported_names.refs ~combine:(fun ~key v1 v2 -> v1)
    ; returns= Map.merge_skewed data.returns imported_names.returns ~combine:(fun ~key v1 v2 -> v1)
    ; members= Map.merge_skewed data.members imported_names.members ~combine:(fun ~key v1 v2 -> v1)
    }
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
  | Import {path; names; info} ->
      let path_name = List.last_exn path in
      let imported_module = Coral_frontend.Import_resolution.Imports.get node imports in
      let imported_names =
        let import_data = NameTraversal.empty () in
        let import_data =
          {import_data with current_scope= data.global_scope; global_scope= data.global_scope}
        in
        run import_data imported_module in
      (* todo: we can defer this by storing links to all imported modules instead of copying them
         over *)
      let data = NameTraversal.merge data imported_names in
      let scope = data.current_scope in
      let scope =
        List.fold ~init:scope
          ~f:(fun scope imp ->
            match imp with
            | Ast.Module (Some module_name) -> Scope.add module_name imported_module scope
            | Ast.Module None -> Scope.add path_name imported_module scope
            | All -> scope (* TODO *)
            | ImpMember (member, Some name) ->
                let imported_member =
                  Map.find_exn imported_names.members {Names.Member.expr= imported_module; member}
                in
                Scope.add name imported_member scope
            | ImpMember (member, None) ->
                let imported_member =
                  Map.find_exn imported_names.members {Names.Member.expr= imported_module; member}
                in
                Scope.add member imported_member scope)
          names in
      {data with current_scope= scope}
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
  | Member {base; member; info} ->
      let data = run data base in
      data
  | Ast.Module {name; lines; info} ->
      (* current_scope starts off in a global or outer scope, so make a new scope for this module *)
      let data = {data with current_scope= Scope.nest data.current_scope} in
      let data = Ast.fold_info ~init:data ~f:run node in
      let data =
        match data.current_scope with
        | Scope {parents; names} ->
            Map.fold names ~init:data ~f:(fun ~key ~data dd ->
                let members =
                  Map.add_exn dd.members ~key:{Names.Member.expr= node; member= key} ~data in
                {dd with members}) in
      data
  | Ast.Type {typ; info} ->
      Ast.show typ |> Stdio.print_endline ;
      Caml.exit 0
  | Ast.TypeDecl {name; metatype; fields; _} ->
      {data with current_scope= Scope.add name node data.current_scope}
  | Ast.TypeAlias {name; typ; _} ->
      {data with current_scope= Scope.add name node data.current_scope}
  | _ -> Ast.fold_info ~init:data ~f:run node

let show n =
  Stdio.printf "Names\n" ;
  Map.iteri n.NameTraversal.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show_short key) (Ast.show_short data)) ;
  Stdio.printf "Names.members\n" ;
  Map.iteri n.NameTraversal.members ~f:(fun ~key ~data ->
      Stdio.printf "    [%s.%s] -> %s\n" (Ast.show_short key.expr) key.member (Ast.show_short data))

let default_global_scope = Builtin_defs.initialize_names ~init:(Scope.create ()) ~f:Scope.add

let construct imports =
  let x = NameTraversal.empty () in
  run imports
    {x with current_scope= default_global_scope; global_scope= default_global_scope}
    imports.Coral_frontend.Import_resolution.Imports.main

let get_data (t : t) = {Names.names= t.refs; returns= t.returns; members= t.members}
