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

(* Builds a Names.t by traversing an Ast *)
let rec run (data : NameTraversal.t) (node : Ast.t) : NameTraversal.t =
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

let resolve_with scope e =
  let x = NameTraversal.empty () in
  run {x with current_scope= scope} e

(* TODO: at some point we should move this to a higher-level library with all the other
   builtin-handling stuff *)
let default_global_scope =
  let open Ast in
  let open Builtins in
  let builtin x = ref @@ Ast.Builtin {builtin= x; info= Info.create ()} in
  let overload name items =
    ref @@ Ast.Overload {name; items= List.map ~f:builtin items; info= Info.create ()} in
  Scope.create ()
  |> Scope.add "Func" @@ builtin @@ FUNC
  |> Scope.add "..." @@ builtin @@ ELLIPSIS
  |> Scope.add "Str" @@ builtin @@ STR
  |> Scope.add "Uint8" @@ builtin @@ UINT8
  |> Scope.add "Uint32" @@ builtin @@ UINT32
  |> Scope.add "Uint64" @@ builtin @@ UINT64
  |> Scope.add "Int8" @@ builtin @@ INT8
  |> Scope.add "Int32" @@ builtin @@ INT32
  |> Scope.add "Int64" @@ builtin @@ INT64
  |> Scope.add "Float64" @@ builtin @@ FLOAT64
  |> Scope.add "Void" @@ builtin @@ VOID
  |> Scope.add "Ptr" @@ builtin @@ PTR
  |> Scope.add "+" (overload "+" [ADD_INT; ADD_FLOAT; ADD_STR])
  |> Scope.add "-" (overload "-" [SUB_INT; SUB_FLOAT])
  |> Scope.add "*" (overload "*" [MUL_INT; MUL_FLOAT])
  |> Scope.add "/" (overload "/" [DIV_INT; DIV_FLOAT])
  |> Scope.add "%" (overload "%" [MOD_INT; MOD_FLOAT])
  |> Scope.add "=" (overload "=" [EQ_INT; EQ_FLOAT])
  |> Scope.add "!=" (overload "!=" [NEQ_INT; NEQ_FLOAT])
  |> Scope.add "<" (overload "<" [LT_INT; LT_FLOAT])
  |> Scope.add ">" (overload ">" [GT_INT; GT_FLOAT])
  |> Scope.add "<=" (overload "<=" [LTE_INT; LTE_FLOAT])
  |> Scope.add ">=" (overload ">=" [GTE_INT; GTE_FLOAT])

let resolve e = resolve_with default_global_scope e
