(* Type Resolver 3

   In this version, we basically do compile-time evaluation, converting an Ast into a set of
   ccvalues. What's the difference between an Ast.node and a ccval? ccvals have types *)

(* avoid being shadowed by Coral_core.Types *)
open Coral_core
open Base

[@@@warning "-26-27"]

module TypeSpec = struct
  (* TypeSpec is the type solver's idea of a "type" It's more like a "compile-time-known
     constraint" *)
  type t =
    | Any
    | ConstInt of int64
    | ConstFloat of float
    | ConstString of string
    | Const of Builtins.t
    | Record of (string option * t) list
    | InstanceOf of t (* e.g. 3 :: InstanceOf Int *)
    | TypeFor of t (* inverse of instanceof -- Int :: TypeFor (Const 3) *)
    | Applied of t * t list
    | And of t * t
    (* And means that multiple typespecs apply to a term.*)
    | Overload of t list
    (* Overload is the OR for typespec. it means a term can be at least one of the
         included typespecs.

         It can be created from an set of overloaded functions
         func foo(a): ...
         @overload
         func foo(b): ...

         This means at any point foo can be one of the following types:
         Func[][A]
         Func[][B]

         Overloaded values can arise from conditionals
         let foo = if rand () then 3 else "3"

         Or also from declaring an untagged union:
         let foo : union { Int; String } = 3

         let foo = (3 | "3")
    *)
    | Error
  [@@deriving compare]

  let rec sexp_of_t = function
    | Const b -> Builtins.sexp_of_t b
    | ConstInt i -> Int64.sexp_of_t i
    | Record fields ->
        let sexp_of_field (name, value) =
          match name with
          | None -> [sexp_of_t value]
          | Some n -> [String.sexp_of_t n; sexp_of_t value] in
        Sexp.List (List.concat_map ~f:sexp_of_field fields)
    | Any -> Sexp.Atom "*"
    | Error -> Sexp.Atom "Error"
    | ConstFloat f -> Float.sexp_of_t f
    | ConstString s -> Atom s
    | InstanceOf x -> List [Atom "instance_of"; sexp_of_t x]
    | TypeFor x -> List [Atom "type_for"; sexp_of_t x]
    | Applied (a, bs) -> List ([Sexp.Atom "Applied"; sexp_of_t a] @ List.map ~f:sexp_of_t bs)
    | And (a, b) -> List [Atom "a"; sexp_of_t a; sexp_of_t b]
    | Overload xs -> List ([Sexp.Atom "Overload"] @ List.map ~f:sexp_of_t xs)

  let rec show = function
    | Any -> "*"
    | InstanceOf t -> "::" ^ show t
    | TypeFor t -> "@@" ^ show t
    | ConstInt x -> Int64.to_string x
    | ConstFloat x -> Float.to_string x
    | ConstString s -> s
    | Const b -> Builtins.show b
    | Record record as r -> "{" ^ Sexp.to_string [%sexp (r : t)] ^ "}"
    | Applied (a, b) -> show a ^ "[" ^ (String.concat ~sep:", " @@ List.map ~f:show b) ^ "]"
    | And (a, b) -> show a ^ " and " ^ show b
    | Overload items -> "overload:(" ^ (String.concat ~sep:"," @@ List.map ~f:show items) ^ ")"
    | Error -> "error"

  let get_type = function
    | TypeFor t -> Const (Builtins.Custom "Type")
    | ConstInt _ -> Const Builtins.INT64
    | ConstString _ -> Const Builtins.STR
    | ConstFloat _ -> Const Builtins.FLOAT64
    | InstanceOf x -> x
    | t -> failwith @@ Sexp.to_string [%sexp "unknown type", (t : t)]

  let of_type x = InstanceOf x
end

module Instance = struct
  (* An instance of an expression with a particular TypeSpec *)
  type t =
    {callee: Ast.t; callee_type: TypeSpec.t; args_types: TypeSpec.t list; result_type: TypeSpec.t}
end

(* an instance is keyed on a callee node * the static types of the arguments *)
module InstanceArgs = struct
  module T = struct type t = Ast.t * TypeSpec.t list [@@deriving compare, sexp_of] end
  include T
  include Comparable.Make (T)
end

module Resolver = struct
  type t =
    { (* we rely on existing name resolution *)
      ns: Coral_core.Names.t
    ; (* a cache for the typespec for an expression *)
      types: TypeSpec.t Map.M(Ast).t
    ; (* known instantiations so far: maps callee -> instance *)
      instantiations: Instance.t Map.M(InstanceArgs).t }

  let add t node type_spec = {t with types= Map.set t.types ~key:node ~data:type_spec}
  let resolve t node = Map.find t.types node

  let dump t =
    Map.mapi t.types ~f:(fun ~key ~data -> [Ast.show_short key; TypeSpec.show data])
    |> Map.to_alist |> List.map ~f:snd |> Utils.show_table
end

let dump = Resolver.dump

let rec construct ns (node : Ast.t) : Resolver.t =
  let t =
    {Resolver.ns; types= Map.empty (module Ast); instantiations= Map.empty (module InstanceArgs)}
  in
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

and check_types t nodes : Resolver.t * TypeSpec.t list = List.fold_map ~init:t ~f:check_type nodes

and match_call t expr : (Resolver.t * TypeSpec.t) Or_error.t =
  let open TypeSpec in
  match expr with
  | Applied (Overload items, args) -> (
      let message =
        Printf.sprintf "Trying to resolve overloads: %s"
          (String.concat ~sep:" " @@ List.map ~f:TypeSpec.show args) in
      match
        List.filter_mapi items ~f:(fun i callee ->
            match_call t (Applied (callee, args)) |> Result.ok)
      with
      | [] -> Or_error.error_s [%sexp "no solutions for overload"]
      | [x] -> Ok x
      | x :: xs -> Ok x
      (* why do we take the first overload? *) )
  | Applied (Const Builtins.ADD_INT, args) ->
      Printf.failwithf "add %s" (String.concat ~sep:", " @@ List.map ~f:TypeSpec.show args) ()
  | Applied (Const Builtins.FUNC, params) -> Ok (t, expr)
  | Applied (Applied (Const Builtins.FUNC, params), return) -> Ok (t, expr)
  | Applied (InstanceOf (Applied (Applied (Const Builtins.FUNC, params), return)), args) ->
      let return_type =
        match return with
        | [] -> Const Builtins.VOID
        | [x] -> x
        | items -> Applied (Const Builtins.TUPLE, items) in
      Ok (t, TypeSpec.of_type return_type)
  | Applied (Const TYPEOF, [arg]) -> Ok (t, TypeSpec.get_type arg)
  | Applied (Const TYPEOF, args) ->
      Ok (t, TypeSpec.Applied (Const TUPLE, List.map ~f:(fun arg -> arg) args))
  | x -> Or_error.error_s [%sexp "failed match_call", (x : TypeSpec.t)]

and check_type_raw (t : Resolver.t) (node : Ast.t) : Resolver.t * TypeSpec.t =
  (* Given an ast node, return the compile-time value of that node.
     Simplified: this gets you what type something is
         e.g. check_type (let x:foo) = ::foo
              check_type 3           = ::Int
              check_type "3"         = ::String
     More in-depth: it actually preserves constant info
              check_type 3           = Const Int 3
              check_type (3 + 4)     = Const Int 7
  *)
  match !node with
  | Ast.Func {name; ret_type; params; body} ->
      let t, param_terms = check_types t params in
      let t, body_term = check_type t body in
      let func_type =
        TypeSpec.(
          Applied
            ( Applied (Const Builtins.FUNC, param_terms)
            , match TypeSpec.get_type body_term with Const VOID -> [] | x -> [x] )) in
      (t, func_type)
  | Ast.Block {items; _} ->
      let t, item_types = check_types t items in
      (t, TypeSpec.(InstanceOf (Const Builtins.VOID)))
  | Ast.Let {name; typ; value; _} ->
      let t = check_type t value in
      t
  | Ast.StringLiteral {literal; _} -> TypeSpec.(t, ConstString literal)
  | Ast.IntLiteral {value; _} -> TypeSpec.(t, ConstInt value)
  | Ast.FloatLiteral {value; _} -> TypeSpec.(t, ConstFloat value)
  | Ast.Call {callee; args; _} ->
      let t, callee_type = check_type t callee in
      let t, args_types = List.fold_map ~init:t ~f:check_type args in
      (* Is callee_type generic? Then we need to create an instance of callee *)
      let instance_type = instantiate t callee callee_type args_types in
      (* Stdio.print_s [%sexp "resolving call", (callee : Ast.Node.t), (args : Ast.Node.t list)] ; *)
      (* match callee_type with
       * | TypeSpec.Const (Builtin FUNC) -> (t, TypeSpec.Applied (callee_type, args_types))
       * | Applied (Const (Builtin FUNC), args) -> (t, TypeSpec.Applied (callee_type, args_types))
       * | _ -> *)
      let sol = match_call t (TypeSpec.Applied (callee_type, args_types)) in
      Or_error.ok_exn sol
  | Ast.Index {callee; args; _} ->
      let t, callee_type = check_type t callee in
      let t, args_types = List.fold_map ~init:t ~f:check_type args in
      let sol = match_call t (TypeSpec.Applied (callee_type, args_types)) in
      Or_error.ok_exn sol
  | Ast.Overload {name; items; _} ->
      let t, item_types = List.fold_map ~init:t ~f:check_type items in
      (t, TypeSpec.Overload item_types)
  | Ast.Extern {name; typ; _} ->
      let t, typ_type = check_type t typ in
      (t, TypeSpec.InstanceOf typ_type)
  | Ast.Member {base; member} ->
      let member_expr =
        let base = Names.deref_var t.ns base |> Option.value ~default:base in
        Option.value_exn
          ~message:("Member reference not found: " ^ Ast.show node)
          (Names.deref_member t.ns base member) in
      check_type t member_expr
  | Ast.TypeAlias {name; typ} -> check_type t typ
  | Ast.TypeDecl {name; metatype; fields} -> (t, ConstString name)
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
  | Ast.Tuple {items= []} -> (t, TypeSpec.Const Builtins.VOID)
  | Ast.Tuple {items} ->
      let t, item_types = List.fold_map ~init:t ~f:check_type items in
      (t, TypeSpec.Record (List.map ~f:(fun x -> (None, x)) item_types))
  | _ -> failwith @@ "unhandled node type in check_type: " ^ Ast.show node

and instantiate t callee callee_type args_types : Resolver.t * TypeSpec.t =
  (* To instantiate a call, we remember the callee and the args_types. (later invocations with the
     same args will be memoized.)

     Then, we must store an "environment" where all the parameters to the function are replaced
     with bound variables of the types of the arguments.

     TODO: we can check if the callee_type is monomorphic to avoid having to do instantiation?
     TODO: we can keep some kind of statistics on call sites so we can generate a type-check and
     thunk.*)
  match Map.find t.instantiations (callee, args_types) with
  | Some existing -> (t, existing.result_type)
  | None ->
      (* Stdio.print_s [%sexp "instantiate", (callee : Ast.t), (args_types : TypeSpec.t list)] ; *)
      let t, new_instance = instantiate1 t callee callee_type args_types in
      let t =
        { t with
          instantiations= Map.set t.instantiations ~key:(callee, args_types) ~data:new_instance }
      in
      (t, new_instance.result_type)

and instantiate1 t callee callee_type args_types : Resolver.t * Instance.t =
  (* Actually instantiate without memoizing the instantiation *)
  (* TODO: this seems to repeat the match that we find in match_call *)
  match callee_type with
  | TypeSpec.Const FUNC ->
      let result_type = TypeSpec.Applied (callee_type, args_types) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | TypeSpec.Applied (Const FUNC, args) ->
      let result_type = TypeSpec.Applied (callee_type, args_types) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | TypeSpec.InstanceOf (Applied (Applied (Const FUNC, param_types), ret_types)) ->
      let result_type =
        match ret_types with
        | [] -> TypeSpec.Const VOID
        | [x] -> x
        | items -> Applied (Const TUPLE, items) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | Const TYPEOF ->
      let result_type = TypeSpec.get_type (List.hd_exn args_types) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | _ -> failwith @@ Sexp.to_string [%sexp "unknown instantiation", {callee_type: TypeSpec.t}]
