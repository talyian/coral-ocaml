(* Type Resolver 3

   In this version, we basically do compile-time evaluation, converting an Ast into a set of
   ccvalues. What's the difference between an Ast.node and a ccval? ccvals have types *)

(* avoid being shadowed by Coral_core.Types *)
open Coral_core
open Base

[@@@warning "-26-27"]

module Instance = struct
  (* An instance of an expression with a particular Typespec *)
  type t =
    {callee: Ast.t; callee_type: Typespec.t; args_types: Typespec.t list; result_type: Typespec.t}
end

(* an instance is keyed on a callee node * the static types of the arguments *)
module InstanceArgs = struct
  module T = struct type t = Ast.t * Typespec.t list [@@deriving compare, sexp_of] end
  include T
  include Comparable.Make (T)
end

module Resolver = struct
  type t =
    { (* we rely on existing name resolution *)
      ns: Coral_core.Names.t
    ; (* a cache for the typespec for an expression *)
      types: Typespec.t Map.M(Ast).t
    ; (* known instantiations so far: maps callee -> instance *)
      instantiations: Instance.t Map.M(InstanceArgs).t }

  let add t node type_spec = {t with types= Map.set t.types ~key:node ~data:type_spec}
  let resolve t node = Map.find t.types node

  let dump t =
    Map.mapi t.types ~f:(fun ~key ~data -> [Ast.show_short key; Typespec.show data])
    |> Map.filter_mapi ~f:(fun ~key ~data ->
           match Ast.Sexp_ref.( ! ) key with
           | Ast.Builtin _ -> None
           | Ast.Index _ -> None
           | _ -> Some data)
    |> Map.to_alist |> List.map ~f:snd |> Utils.show_table
end

let dump = Resolver.dump

(* gets the type of a node *)
let get_type resolver node = (resolver, Typespec.get_type node)

(* fst @@ check_type resolver
 * @@ Option.value_exn ~message:"Main func not found"
 *      (Names.deref_member resolver.ns node "main") *)
let rec check_type t node : Resolver.t * Typespec.t =
  (* memoized *)
  match Resolver.resolve t node with
  | Some type_spec -> (t, type_spec)
  | None ->
      let t, type_spec = check_type_raw t node in
      let t = Resolver.add t node type_spec in
      (t, type_spec)

and check_types t nodes : Resolver.t * Typespec.t list = List.fold_map ~init:t ~f:check_type nodes

and match_call t (expr, args) : (Resolver.t * Typespec.t) Or_error.t =
  let open Typespec in
  match (expr, args) with
  | Overload items, args -> (
      let message =
        Printf.sprintf "Trying to resolve overloads: %s"
          (String.concat ~sep:" " @@ List.map ~f:Typespec.show args) in
      match
        List.filter_mapi items ~f:(fun i callee -> match_call t (callee, args) |> Result.ok)
      with
      | [] -> Or_error.error_s [%sexp "no solutions for overload"]
      | [x] -> Ok x
      | x :: xs -> Ok x
      (* why do we take the first overload? *) )
  | Const Builtins.ADD_INT, args ->
      Or_error.errorf "add %s" (String.concat ~sep:", " @@ List.map ~f:Typespec.show args)
  | Const Builtins.ADD_PTR_INT, args -> Ok (t, Typespec.Any)
  | Const Builtins.PTR, params -> Ok (t, expr)
  | Const Builtins.FUNC, params -> Ok (t, expr)
  | Applied (Const Builtins.FUNC, params), return -> Ok (t, expr)
  | InstanceOf (Applied (Applied (Const Builtins.FUNC, params), return)), args ->
      let return_type =
        match return with
        | [] -> Typespec.instance_of @@ Const Builtins.VOID
        | [x] -> x
        | items -> Applied (Const Builtins.TUPLE, items) in
      Ok (t, return_type)
  | Const TYPEOF, [arg] -> Ok (t, Typespec.get_type arg)
  | Const TYPEOF, args -> Ok (t, Typespec.Applied (Const TUPLE, List.map ~f:(fun arg -> arg) args))
  | x ->
      Or_error.error_s [%sexp "failed match_call", (expr : Typespec.t), (args : Typespec.t list)]

and check_type_raw (t : Resolver.t) (node : Ast.t) : Resolver.t * Typespec.t =
  (* Given an ast node, return the compile-time value of that node.
     Simplified: this gets you what type something is
         e.g. check_type (let x:foo) = ::foo
              check_type 3           = ::Int
              check_type "3"         = ::String
     More in-depth: it actually preserves constant info
              check_type 3           = Const Int 3
              check_type (3 + 4)     = Const Int 7
  *)
  match Ast.Sexp_ref.( ! ) node with
  | Ast.Func {name; ret_type; params; body} ->
      let t, param_terms = check_types t params in
      let t, body_term = check_type t body in
      let func_type =
        Typespec.(
          Applied
            ( Applied (Const Builtins.FUNC, param_terms)
            , match Typespec.get_type body_term with Const VOID -> [] | x -> [x] )) in
      (t, func_type)
  | Ast.Block {items; _} ->
      let t, item_types = check_types t items in
      (t, Typespec.(InstanceOf (Const Builtins.VOID)))
  | Ast.Let {name; typ; value; _} ->
      let t = check_type t value in
      t
  | Ast.StringLiteral {literal; _} -> Typespec.(t, ConstString literal)
  | Ast.IntLiteral {value; _} -> Typespec.(t, ConstInt value)
  | Ast.FloatLiteral {value; _} -> Typespec.(t, ConstFloat value)
  | Ast.Call {callee; args; _} ->
      let t, callee_type = check_type t callee in
      let t, args_types = List.fold_map ~init:t ~f:check_type args in
      (* Is callee_type generic? Then we need to create an instance of callee *)
      let instance_type = instantiate_call_memo t callee callee_type args_types in
      (* Stdio.print_s [%sexp "resolving call", (callee : Ast.Node.t), (args : Ast.Node.t list)] ; *)
      (* match callee_type with
       * | Typespec.Const (Builtin FUNC) -> (t, Typespec.Applied (callee_type, args_types))
       * | Applied (Const (Builtin FUNC), args) -> (t, Typespec.Applied (callee_type, args_types))
       * | _ -> *)
      let sol = match_call t (callee_type, args_types) in
      Or_error.ok_exn sol
  | Ast.Index {callee; args; _} ->
      let t, callee_type = check_type t callee in
      let t, args_types = List.fold_map ~init:t ~f:check_type args in
      (* in the coral language, x:Foo[Bar] means (x is of type (applied(foo, [bar])))
         in the typespec language, this actually translates into x = is_type(applied(foo, [is_type bar])).
         (extra is_type on the bar).
         This is because typespec wants to keep all the term-level info for the parameters. *)
      let args_types = List.map ~f:(fun t -> Typespec.InstanceOf t) args_types in
      (* TODO: how to tell if callee_type is a parameterized type constructor? check its get_type
         if callee_type is a parameterized type constructor then we can produce an applied *)
      (t, Typespec.Applied (callee_type, args_types))
  | Ast.Overload {name; items; _} ->
      let t, item_types = List.fold_map ~init:t ~f:check_type items in
      (t, Typespec.Overload item_types)
  | Ast.Extern {name; typ; _} ->
      let t, typ_type = check_type t typ in
      (* Stdio.print_s [%sexp "extern", {name: string; typ: Ast.t; typ_type: Typespec.t}] ; *)
      (t, Typespec.InstanceOf typ_type)
  | Ast.Member {base; member} -> (
      let base = Names.deref_var t.ns base |> Option.value ~default:base in
      (* foo.bar might be statically resolved by name resolution if foo is a
         product type / module etc. *)
      match Names.deref_member t.ns base member with
      | Some member_expr -> check_type t member_expr
      | None ->
          let t, base_type = check_type t base in
          (* foo.bar might refer to type-directed field lookup *)
          (* foo.bar might refer to type-directed method lookup *)
          failwith @@ "unhandled member type: ^ " ^ Ast.show node )
  | Ast.TypeAlias {name; typ} -> check_type t typ
  | Ast.TypeDecl {name; metatype= "struct"; fields} ->
      (* TODO: the type of a typedecl T has the following properties:
         T.type == Type (* is this true? what about parameterized types? *)
         T.fields == fields
         T.name == name
         T.constructor = Func[...fields...][T]
      *)
      (* This is recursive, because T.constructor needs to reference T. *)
      Stdio.print_s [%sexp "fields", (fields : Ast.t list)] ;
      let t, fields = List.fold_map ~init:t ~f:check_type fields in
      let this_type =
        let field_type = Typespec.Const (Builtins.Custom "Type") in
        let field_fields = [] in
        let field_name = Typespec.ConstString name in
        (* let field_constructor_type =
         *   Typespec.(
         *     let func = Const Builtins.FUNC in
         *     Applied (Applied (func, fields), [this_type])) in *)
        Typespec.Record
          [ (Some "name", field_name)
          ; (Some "fields", Applied (Const (Custom "List"), field_fields))
          ; (Some "type", field_type) (* (Some "constructor_type", field_constructor_type) *)
          ; (Some "metatype", Const (Custom "struct")) ] in
      (t, this_type)
  | Ast.TypeDecl {name; metatype; fields} -> (t, ConstString ("TODO: type-" ^ name))
  | Ast.Var {name; _} ->
      let reference =
        Option.value_exn ~message:("name not found: " ^ name) (Names.deref_var t.ns node) in
      let res, typ = check_type t reference in
      (* Stdio.print_s [%sexp "check-type (var)", {name: string; reference: Ast.t; typ: Typespec.t}] ; *)
      (res, typ)
  | Ast.Builtin {builtin; _} -> (t, Typespec.Const builtin)
  | Ast.Param {idx; name; typ; _} -> (
    match typ with
    | Some typ ->
        let t, typ_type = check_type t typ in
        Typespec.(t, InstanceOf typ_type)
    | None -> (t, Typespec.Any) )
  | Ast.Tuple {items= []} -> (t, Typespec.instance_of @@ Typespec.Const Builtins.VOID)
  | Ast.Tuple {items} ->
      let t, item_types = List.fold_map ~init:t ~f:check_type items in
      (t, Typespec.Record (List.map ~f:(fun x -> (None, x)) item_types))
  | Ast.Import _ -> (t, Typespec.Any)
  | Ast.Module {lines; _} ->
      let t, item_types = List.fold_map ~init:t ~f:check_type lines in
      (t, Typespec.Any)
  | _ -> failwith @@ "unhandled node type in check_type: " ^ Ast.show node

and instantiate_call_memo t callee callee_type args_types : Resolver.t * Typespec.t =
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
      (* Stdio.print_s [%sexp "instantiate_call_memo", (callee : Ast.t), (args_types : Typespec.t list)] ; *)
      let t, new_instance = instantiate_call t callee callee_type args_types in
      let t =
        { t with
          instantiations= Map.set t.instantiations ~key:(callee, args_types) ~data:new_instance }
      in
      (t, new_instance.result_type)

and instantiate_call t callee callee_type args_types : Resolver.t * Instance.t =
  (* Actually instantiate without memoizing the instantiation *)
  (* TODO: this seems to repeat the match that we find in match_call *)
  match callee_type with
  | Typespec.Const FUNC ->
      let result_type = Typespec.Applied (callee_type, args_types) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | Typespec.Applied (Const FUNC, args) ->
      let result_type = Typespec.Applied (callee_type, args_types) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | Typespec.InstanceOf (Applied (Applied (Const FUNC, param_types), ret_types)) ->
      let result_type =
        match ret_types with
        | [] -> Typespec.instance_of @@ Typespec.Const VOID
        | [x] -> x
        | items -> Applied (Const TUPLE, items) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | Const TYPEOF ->
      let result_type = Typespec.get_type (List.hd_exn args_types) in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | Overload items ->
      let result_type = Typespec.Error in
      (t, Instance.{callee; callee_type; args_types; result_type})
  | callee_type ->
      dump t ;
      Stdio.print_endline
      @@ Sexp.to_string_hum
           [%sexp
             "TODO: unknown instantiation"
             , {callee: Ast.t; callee_type: Typespec.t; args_types: Typespec.t list}] ;
      failwith "unknown instantiation"

(* entry point -- runs check_type on the module *)
let construct ns (node : Ast.t) : Resolver.t =
  let resolver =
    {Resolver.ns; types= Map.empty (module Ast); instantiations= Map.empty (module InstanceArgs)}
  in
  match Names.deref_member resolver.ns node "main" with
  | Some main -> fst @@ check_type resolver main
  | None -> fst @@ check_type resolver node

module Tests = struct
  let%expect_test "call resolution" =
    (* These are the types of TODO what are these? reduction rules? *)
    (* Type Application - given x:T, a:A => Call(x, a):T[A] *)
    (* type T
       type A
       type F = T(A) *)
    (* Function call - given f:Func[..A][R], and ..a:..A then Call(f, ..a):R
       an instance of a function is of type InstanceOf(Appl(Appl(FUNC, ...args...), [ret])) *)
    (* type A
       type B
       type R
       type F = Func[A, B][R]
       let x : F
       let a = _, b = _, c = x(a, b)
    *)
    ()
end
[@@expect_module]
