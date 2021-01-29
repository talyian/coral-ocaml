(* Type Resolver 3

   In this version, we basically do compile-time evaluation, converting an Ast into a set of
   ccvalues. What's the difference between an Ast.node and a ccval? ccvals have types *)

(* evoid being shadowed by Coral_core.Types *)
open Coral_core
open Base

let show_table list =
  let lengths = List.init ~f:(Fn.const 0) (List.length @@ List.hd_exn list) in
  let lengths =
    List.fold ~init:lengths
      ~f:(fun lengths row ->
        List.zip_exn lengths row |> List.map ~f:(fun (a, b) -> max a (String.length b)))
      list in
  (List.iter
     ~f:(fun row ->
       List.iteri row ~f:(fun i cell ->
           Stdio.print_string
           @@ String.prefix (cell ^ String.make 100 ' ') (1 + List.nth_exn lengths i)) ;
       Stdio.print_endline "")
     list [@warning "-8"])

module TypeSpec = struct
  type t =
    | Any
    | ConstStr of string
    | Const of Builtins.t
    | InstanceOf of t
    | TypeFor of t
    | Applied of t * t list
    | And of t * t

  let rec show = function
    | Any -> "*"
    | ConstStr s -> s
    | Const b -> Builtins.show b
    | InstanceOf t -> "::" ^ show t
    | TypeFor t -> "@@" ^ show t
    | Applied (a, b) -> show a ^ "[" ^ (String.concat ~sep:", " @@ List.map ~f:show b) ^ "]"
    | And (a, b) -> show a ^ " and " ^ show b
end

module Resolver = struct
  type t = {ns: Coral_core.Names.t; types: TypeSpec.t Map.M(Ast).t}

  let add t node type_spec = {t with types= Map.set t.types ~key:node ~data:type_spec}
  let resolve t node = Map.find t.types node

  let dump t =
    Map.mapi t.types ~f:(fun ~key ~data -> [Ast.show_short key; TypeSpec.show data])
    |> Map.to_alist |> List.map ~f:snd |> show_table
end

let rec construct ns (node : Ast.t) : Resolver.t =
  let t = {Resolver.ns; types= Map.empty (module Ast)} in
  fst @@ check_type t
  @@ Option.value_exn ~message:"Main func not found" (Names.deref_member t.ns node "main")

and check_type t node : Resolver.t * TypeSpec.t =
  (* memoized *)
  match Resolver.resolve t node with
  | Some type_spec -> (t, type_spec)
  | None ->
      let t, type_spec = check_type_raw t node in
      let t = Resolver.add t node type_spec in
      (t, type_spec)

and check_type_raw (t : Resolver.t) node : Resolver.t * TypeSpec.t =
  match !node with
  | Ast.Func {name; ret_type; params; body; info} ->
      let t, param_types = List.fold_map ~init:t ~f:check_type params in
      let t, body_type = check_type t body in
      let func_type =
        TypeSpec.(Applied (Applied (Const Builtins.FUNC, [body_type]), param_types)) in
      (t, func_type)
  | Ast.Block {items; _} ->
      let t, item_types = List.fold_map ~init:t ~f:check_type items in
      (t, TypeSpec.(InstanceOf (Const Builtins.VOID)))
  | Ast.Let {name; typ; value; _} ->
      let t = check_type t value in
      t
  | Ast.StringLiteral _ -> TypeSpec.(t, InstanceOf (Const Builtins.STR))
  | Ast.IntLiteral _ -> TypeSpec.(t, InstanceOf (Const Builtins.INT64))
  | Ast.FloatLiteral _ -> TypeSpec.(t, InstanceOf (Const Builtins.FLOAT64))
  | Ast.Call {callee; args= [{contents= Ast.List {items= args; _}}]; _}
   |Ast.Index {callee; args; _}
   |Ast.Call {callee; args; _} ->
      let t, callee_type = check_type t callee in
      let t, args_types = List.fold_map ~init:t ~f:check_type args in
      let typ =
        TypeSpec.(
          match Applied (callee_type, args_types) with
          | Applied (Applied (Applied (Const Builtins.FUNC, return), params), args) ->
              Stdio.printf "call of func type:\n  %s\n  %s\n  %s\n"
                (List.map ~f:show return |> String.concat ~sep:"; ")
                (List.map ~f:show params |> String.concat ~sep:"; ")
                (List.map ~f:show args |> String.concat ~sep:"; ") ;
              Stdio.printf "TODO: unify args and params\n" ;
              let return_type =
                match return with
                | [] -> Const Builtins.VOID
                | [x] -> x
                | items -> Applied (Const Builtins.TUPLE, items) in
              return_type
          | x -> x) in
      (t, typ)
  | Ast.Extern {name; typ; _} ->
      let t, typ_type = check_type t typ in
      (t, TypeSpec.InstanceOf typ_type)
  | Ast.Member {base; member; info} ->
      let member_expr =
        let base = Names.deref_var t.ns base |> Option.value ~default:base in
        Option.value_exn
          ~message:("Member reference not found: " ^ Ast.show node)
          (Names.deref_member t.ns base member) in
      check_type t member_expr
  | Ast.TypeAlias {name; typ; info} -> check_type t typ
  | Ast.TypeDecl {name; metatype; fields; info} -> (t, TypeSpec.ConstStr name)
  | Ast.Var {name; _} ->
      let reference =
        Option.value_exn ~message:("name not found: " ^ name) (Names.deref_var t.ns node) in
      let t = check_type t reference in
      t
  | Ast.Builtin {builtin; _} -> (t, TypeSpec.Const builtin)
  | Ast.Param {idx; name; typ; _} -> (
    match typ with
    | Some typ ->
        let t, typ_type = check_type t typ in
        TypeSpec.(t, InstanceOf typ_type)
    | None -> (t, TypeSpec.Any) )
  | Ast.Tuple {items= []; _} -> (t, TypeSpec.Const Builtins.VOID)
  | _ -> failwith @@ "unhandled node type in check_type: " ^ Ast.show node
