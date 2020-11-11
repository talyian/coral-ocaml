open Base

module Scope = struct
  (* A target is info about an expression referred to by a variable. To support overloading, it
     contains information about the containing scope and an overloadable flag *)
  type target = {expr: Ast.Node.t; container: Ast.Node.t; overloadable: bool}

  (* A scope is a lexical scope that contains a mapping of string names to nodes let x = 1 loop:
     let y = "2" <-- here the scope would be {"y" -> Let(y, 2), parents=[{"x" -> Let(x, 1)}]}

     It is used during name resolution to build the lexical structure of variable references *)
  type scope = {parent: scope list; names: (String.t, target, String.comparator_witness) Base.Map.t}
  type t = scope

  let empty = {parent= []; names= Map.empty (module String)}
  let nest parent = {empty with parent= [parent]}

  let add key data scope =
    let overloadable = match fst data with Ast.Func _ -> true | _ -> false in
    let existing = Map.find scope.names key in
    match (overloadable, existing) with
    | true, Some {expr; overloadable= true; container= _} ->
        Stdio.printf "Overloading! %s\n" (Ast.nodeName (fst data)) ;
        let data = Ast.Make.overload expr data in
        let data = {expr= data; container= Ast.mm @@ Ast.Empty; overloadable} in
        {scope with names= Map.set ~key ~data scope.names}
    | true, Some {expr; _} ->
        failwith @@ "cannot overload nonoverloadable expr " ^ Ast.show_node expr
    | _ ->
        let data = {expr= data; container= Ast.mm @@ Ast.Empty; overloadable} in
        {scope with names= Map.add_exn ~key ~data scope.names}

  (* Used during name resolution: name:string -> scope -> target *)
  let rec find_extra ~name scope =
    match Map.find scope.names name with
    | Some v -> Some v
    | None -> List.find_map ~f:(find_extra ~name) scope.parent

  (* Used during name resolution: name:string -> scope -> Ast.node *)
  let find ~name scope = Option.map ~f:(fun x -> x.expr) (find_extra ~name scope)
end

(* Names.t names a nameresolver with a mapping of refs from Vars to Exprs that they reference *)
module Names = struct
  type t =
    { current_scope: Scope.t (* used internally *)
    ; refs: Ast.Node.t Ast.Node.Map.t (* maps Vars to references *)
    ; type_refs: Ast.Node.t Ast.Type.Map.t (* maps named types to references *)
    ; returns: Ast.Node.t Ast.Node.Map.t (* maps a return to its function *) }

  let empty =
    { current_scope= Scope.empty
    ; refs= Map.empty (module Ast.Node)
    ; type_refs= Map.empty (module Ast.Type)
    ; returns= Map.empty (module Ast.Node) }

  let deref t node = Map.find t.refs node
  let deref_or_self t node = Some (Option.value ~default:node (Map.find t.refs node))
end

include Names
