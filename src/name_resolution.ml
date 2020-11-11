open Coral_core
open Base
module Scope = Names.Scope

(* Builds a Names.t by traversing an Ast *)
let rec run (data : Names.t) node : Names.t =
  let e, _ = node in
  match e with
  | Ast.Var {name; _} ->
      let reference = Scope.find ~name data.current_scope in
      let reference = Option.value_exn ~message:("Name not found: " ^ name) reference in
      {data with refs= Map.add_exn data.refs ~key:node ~data:reference}
  | Ast.Extern {binding= _; name; typ} ->
      let data = fold_type data typ in
      let scope = Scope.add name node data.current_scope in
      {data with current_scope= scope}
  | Ast.Func {name; params; body; _} ->
      let outer_scope = Scope.add name node data.current_scope in
      let inner_scope = Scope.nest outer_scope in
      let data = {data with current_scope= inner_scope} in
      let data = List.fold params ~init:data ~f:run in
      let data = run data body in
      {data with current_scope= outer_scope}
  | Ast.Param {name; typ; idx= _} ->
      let data = Option.fold ~f:fold_type ~init:data typ in
      {data with current_scope= Scope.add name node data.current_scope}
  | Ast.Let (var, value) ->
      let outer_scope = data.current_scope in
      let data = Option.fold ~f:fold_type ~init:data var.varType in
      let data = run {data with current_scope= Scope.nest data.current_scope} value in
      let data = {data with current_scope= outer_scope} in
      let scope = Scope.add var.name node data.current_scope in
      {data with current_scope= scope}
  | _ -> Ast.fold ~init:data ~f:run node

and fold_type data typ : Names.t =
  match typ with
  | Type.Name name ->
      let reference = Scope.find ~name data.current_scope in
      let reference = Option.value_exn ~message:("Name not found: " ^ name) reference in
      {data with type_refs= Map.add_exn data.type_refs ~key:typ ~data:reference}
  | _ -> Ast.Type.fold ~init:data ~f:fold_type typ

let show n =
  Map.iteri n.Names.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show_node key) (Ast.nodeName (fst data))) ;
  Map.iteri n.Names.type_refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Type.show Ast.pp_node key) (Ast.nodeName (fst data)))

let resolve e =
  let global_scope =
    let open Ast in
    let open Builtins in
    let overload name items =
      mm @@ Overload {name; items= List.map ~f:(fun x -> mm @@ Builtin x) items} in
    Scope.empty
    |> Scope.add "Func" (mm @@ Builtin FUNC)
    |> Scope.add "..." (mm @@ Builtin ELLIPSIS)
    |> Scope.add "Str" (mm @@ Builtin STR)
    |> Scope.add "Int64" (mm @@ Builtin INT64)
    |> Scope.add "Float64" (mm @@ Builtin FLOAT64)
    |> Scope.add "+" (overload "+" [ADD_INT; ADD_FLOAT; ADD_STR])
    |> Scope.add "-" (overload "-" [SUB_INT; SUB_FLOAT])
    |> Scope.add "*" (overload "-" [MUL_INT; MUL_FLOAT])
    |> Scope.add "/" (overload "-" [DIV_INT; DIV_FLOAT])
    |> Scope.add "%" (overload "-" [MOD_INT; MOD_FLOAT])
    |> Scope.add "=" (overload "-" [EQ_INT; EQ_FLOAT])
    |> Scope.add "!=" (overload "-" [NEQ_INT; NEQ_FLOAT])
    |> Scope.add "<" (overload "-" [LT_INT; LT_FLOAT])
    |> Scope.add ">" (overload "-" [GT_INT; GT_FLOAT])
    |> Scope.add "<=" (overload "-" [LTE_INT; LTE_FLOAT])
    |> Scope.add ">=" (overload "-" [GTE_INT; GTE_FLOAT]) in
  Stdio.print_endline @@ Sexp.to_string_hum @@ Ast.Node.sexp_of_t e ;
  let x = run {Names.empty with current_scope= global_scope} e in
  x
