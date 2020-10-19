open Coral_core
open Base

(* A scope is a lexical scope that contains a mapping of string names to nodes let x = 1 loop: let
   y = "2" <-- here the scope would be {"y" -> Let(y, 2), parents=[{"x" -> Let(x, 1)}]} *)

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

  let deref (data : t) node = Map.find data.refs node
end

let rec run (data : Names.t) node : Names.t =
  let e, _ = node in
  Stdio.printf "NS:run %s\n" (Ast.nodeName e);
  match e with
  | Ast.Var { name; _ } ->
      let reference = Scope.find ~name data.current_scope in
      let reference = Option.value_exn ~message:("Name not found: " ^ name) reference in
      { data with refs = Map.add_exn data.refs ~key:node ~data:reference }
  | Ast.Extern { name; _ } ->
      let scope = Scope.add name node data.current_scope in
      { data with current_scope = scope }
  | Ast.Func { name; params; body; _ } ->
      let outer_scope = Scope.add name node data.current_scope in
      let inner_scope = Scope.nest outer_scope in
      let data = { data with current_scope = inner_scope } in
      let data = List.fold params ~init:data ~f:run in
      let data = run data body in
      { data with current_scope = outer_scope }
  | Ast.Param { name; _ } -> { data with current_scope = Scope.add name node data.current_scope }
  | Ast.Let (var, value) ->
      let outer_scope = data.current_scope in
      let data = run { data with current_scope = Scope.nest data.current_scope } value in
      let data = { data with current_scope = outer_scope } in
      let scope = Scope.add var.name node data.current_scope in
      { data with current_scope = scope }
  | _ -> Ast.fold ~init:data ~f:run node

let show n =
  Map.iteri n.Names.refs ~f:(fun ~key ~data ->
      Stdio.printf "    [%s] -> %s\n" (Ast.show_node key) (Ast.nodeName (fst data)))

let resolve e =
  let global_scope =
    let open Ast in
    let open Builtins in
    let overload name items =
      mm @@ Overload { name; items = List.map ~f:(fun x -> mm @@ Builtin x) items }
    in
    Scope.empty
    |> Scope.add "+" (overload "+" [ ADD_INT; ADD_FLOAT ])
    |> Scope.add "-" (overload "-" [ SUB_INT; SUB_FLOAT ])
    |> Scope.add "*" (overload "-" [ MUL_INT; MUL_FLOAT ])
    |> Scope.add "/" (overload "-" [ DIV_INT; DIV_FLOAT ])
    |> Scope.add "%" (overload "-" [ MOD_INT; MOD_FLOAT ])
    |> Scope.add "=" (overload "-" [ EQ_INT; EQ_FLOAT ])
    |> Scope.add "!=" (overload "-" [ NEQ_INT; NEQ_FLOAT ])
    |> Scope.add "<" (overload "-" [ LT_INT; LT_FLOAT ])
    |> Scope.add ">" (overload "-" [ GT_INT; GT_FLOAT ])
    |> Scope.add "<=" (overload "-" [ LTE_INT; LTE_FLOAT ])
    |> Scope.add ">=" (overload "-" [ GTE_INT; GTE_FLOAT ])
  in
  Stdio.print_endline @@ Sexp.to_string_hum @@ Ast.Node.sexp_of_t e;
  let x = run { Names.empty with current_scope = global_scope } e in
  x
