(* Type Resolver 3

   In this version, we basically do compile-time evaluation, converting an Ast into a set of
   ccvalues. What's the difference between an Ast.node and a ccval? ccvals have types *)
open Coral_core
open Base

module Cconst = struct
  type t =
    | Bool of bool
    | Int8 of char
    | Int32 of int32
    | Int64 of int64
    | Float64 of float
    | String of string
  [@@deriving equal]

  let show = function
    | Bool b -> Bool.to_string b
    | Int8 c -> Char.to_string c
    | Int32 i -> Int32.to_string i
    | Int64 i -> Int64.to_string i
    | Float64 f -> Float.to_string f
    | String s -> "\"" ^ String.escaped s ^ "\""
end

module Ccval = struct
  type t =
    | Error (* this represents a type error, always false *)
    | Unknown (* this represents a unknown variable, always true *)
    (* This represents a set of possibilities, of which we accept the first match *)
    | Overload of t list
    | Const of Cconst.t
    | Forall of int * t (* a universal quantifier - introduces a scope for a type variable *)
    | TypeVar of int
    (* a free type variable introduced within the scope of a universal quantifier *)
    | Is of Builtins.t
    | Appl of t * t list
    | IsInstance of t
    | TypeOf of t
    | Call of t * t list
    | Tuple of t list
    | And of t * t
    | OverloadIndex of int
  [@@deriving equal]

  (* | Lambda of {
   *     params: y list;
   *     openvars: y list;
   *     body: y
   *   } *)
  (* | Block of y list *)

  let rec show v =
    let cc x = String.concat ~sep:", " @@ List.map ~f:show x in
    match v with
    | Error -> "err"
    | Unknown -> "???"
    | Const c -> Cconst.show c
    | Overload o -> Printf.sprintf "ov.{%s}" (cc o)
    | Forall (x, body) -> Printf.sprintf "∀.%d[%s]" x (show body)
    | TypeVar x -> Printf.sprintf "τ%d" x
    | IsInstance (Is Builtins.VOID) -> "()"
    | Is Builtins.VOID -> "void"
    | Is b -> Builtins.show b
    | Appl (x, args) -> Printf.sprintf "%s[%s]" (show x) (cc args)
    | Call (x, args) -> Printf.sprintf "%s(%s)" (show x) (cc args)
    | IsInstance x -> "::" ^ show x
    | TypeOf x -> "@" ^ show x
    | Tuple x -> "(" ^ cc x ^ ")"
    | And (a, b) -> show a ^ " && " ^ show b
    | OverloadIndex i -> "ov." ^ Int.to_string i

  (* | Lambda {params; openvars=_; body} -> Printf.sprintf "λ.%s(%s)" (cc params) (show body) *)

  let type_of = function
    | Tuple [] -> Is Builtins.VOID
    | IsInstance x -> x
    | Const (Cconst.Bool _) -> Is Builtins.BOOL
    | Const (Cconst.Int8 _) -> Is Builtins.INT8
    | Const (Cconst.Int32 _) -> Is Builtins.INT32
    | Const (Cconst.Int64 _) -> Is Builtins.INT64
    | Const (Cconst.Float64 _) -> Is Builtins.FLOAT64
    | Const (Cconst.String _) -> Is Builtins.STR
    | x -> TypeOf x

  let rec is_instance = function
    | TypeOf x -> x
    | Tuple ts -> Tuple (List.map ~f:is_instance ts)
    | x -> IsInstance x

  let make_tuple = function [] -> Is Builtins.VOID | [ x ] -> x | xs -> Tuple xs
end

type ccval = Ccval.t

type cconst = Cconst.t

(** A resolver is the data for a type resolution compilation pass. It follows name resolution (and
    relies on Name resolver ) and produces a map of Ast.Node -> ccval for later stages *)
module Resolver = struct
  type t = {
    ns : Name_resolution.Names.t;
    memo : ccval Ast.Node.Map.t;
    instantiations : ccval Map.M(Int).t;
    overload_index : int Ast.Node.Map.t;
  }

  let with_mapping (id : int) ccval res = res

  let with_overload node idx res =
    { res with overload_index = Map.set ~key:node ~data:idx res.overload_index }

  let add node data resolver = { resolver with memo = Map.set ~key:node ~data resolver.memo }
end

type resolver = Resolver.t

(* let rec unify env (x : ccval) (y : ccval) = if Ccval.equal x y then (env, x) else _unify env x y
 *
 * and _unify env x y =
 *   let open Ccval in
 *   match (x, y) with
 *   | TypeVar x, TypeVar y -> bind (TypeVar x) (TypeVar y) env
 *   | TypeVar x, y -> bind (TypeVar x) y env
 *   | x, TypeVar y -> bind (TypeVar y) x env
 *   | x, y -> failwith @@ Printf.sprintf "cannot unify %s and %s" (Ccval.show x) (Ccval.show y) *)

(* let unify_call (environment : t) params args ret =
 *   let rec unifyarg (env : t) = function
 *     | [], [] -> env
 *     | x :: xs, y :: ys ->
 *         let env, z = unify env x y in
 *         unifyarg env (xs, ys)
 *     | _ -> failwith "unify: argument count mismatch"
 *   in
 *   let env = unifyarg environment (params, args) in
 *   let ret = eval_in env ret in
 *   (env, List.nth_exn ret 0) *)

let rec resolve_call (resolver : resolver) call callee (args : ccval list) =
  let open Ccval in
  (* Stdio.printf "resolve_call %s\n" (Ccval.show callee); *)
  match callee with
  | IsInstance (Appl (Appl (Is Builtins.VARFUNC, ret), _)) ->
      (resolver, is_instance @@ make_tuple ret)
  | IsInstance (Appl (Appl (Is Builtins.FUNC, ret), params)) ->
      List.iter2_exn params args ~f:(fun param arg ->
          let ptype = param in
          let atype = Ccval.type_of arg in
          if Ccval.equal ptype atype then ()
          else (
            Stdlib.flush_all ();
            failwith
            @@ Printf.sprintf "value mismatch %s <-> %s" (Ccval.show ptype) (Ccval.show atype) ));
      (resolver, is_instance @@ make_tuple ret)
  | Overload items ->
      (* When calling an overload set, try all the cases until one fits *)
      (* tag the call expr with the overload index as well *)
      let rec resolve_overload res i = function
        | [] -> (res, Unknown) (* no overload matched *)
        | x :: xs -> (
            match resolve_call res call x args with
            | exception _ -> resolve_overload res (i + 1) xs
            | res, ret_val ->
                let res = Resolver.with_overload call i res in
                (res, ret_val) )
      in
      resolve_overload resolver 0 items
  | IsInstance (Forall (_t, Appl (Appl (Is Builtins.FUNC, ret), params))) ->
      (* if a call is to a universally quantified type, we need to unify it against the arguments.
         We need to instantiate the quantified variable *)
      let tvar = Ccval.Unknown in
      let resolver = Resolver.with_mapping _t tvar resolver in
      let resolver =
        List.fold2_exn
          ~f:(fun res arg param ->
            Stdio.printf "---------- unifying call: %s : %s\n" (Ccval.show arg) (Ccval.show param);
            (* TODO: universal quantification *)
            (* arg param *)
            res)
          ~init:resolver args params
      in
      (* let env = Environment.(empty |> add_var _t) in *)
      (* let env, out = Environment.unify_call env params args ret in *)
      let out = Ccval.Unknown in
      (resolver, out)
  | _ ->
      Stdio.eprintf "unknown call situation %s (%s)\n" (Ccval.show callee)
        (String.concat ~sep:", " @@ List.map ~f:Ccval.show args);
      failwith "unknown call"

exception Ellipsis

let rec resolve_type resolver typ =
  let open Ccval in
  match typ with
  | Type.Name "Int64" -> (resolver, Is Builtins.INT64)
  | Type.Name "Func" -> (resolver, Is Builtins.FUNC)
  | Type.Name "Str" -> (resolver, Is Builtins.STR)
  | Type.Name "..." -> raise Ellipsis
  | Type.Parameterized (Type.Parameterized (Type.Name "Func", ret), params) -> (
      try
        let resolver, bs = List.fold_map ~f:resolve_type ~init:resolver params in
        let resolver, cs = List.fold_map ~f:resolve_type ~init:resolver ret in
        let resolver, a = resolve_type resolver (Type.Name "Func") in
        (resolver, Appl (Appl (a, cs), bs))
      with Ellipsis ->
        let params = List.filter ~f:(function Type.Name "..." -> false | _ -> true) params in
        let resolver, bs = List.fold_map ~f:resolve_type ~init:resolver params in
        let resolver, cs = List.fold_map ~f:resolve_type ~init:resolver ret in
        let a = Is Builtins.VARFUNC in
        (resolver, Appl (Appl (a, cs), bs)) )
  | Type.Parameterized (a, bs) ->
      let resolver, bs = List.fold_map ~f:resolve_type ~init:resolver bs in
      let resolver, a = resolve_type resolver a in
      (resolver, Appl (a, bs))
  | t -> failwith @@ "unknown type" ^ Type.show Ast.pp_node t

let rec _resolve resolver node =
  let open Ccval in
  match fst node with
  | Ast.Func { params; body; _ } ->
      (* TODO: we really should be doing something particular with return x instead of just
         evaluating it the same as x *)
      let resolver, ccparams = List.fold_map ~init:resolver ~f:resolve params in
      let ccparams = List.map ~f:Ccval.type_of ccparams in
      (* to break infinite recursion, we need to add a term here *)
      let resolver = Resolver.add node Unknown resolver in
      let resolver, ccbody = resolve resolver body in
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [ Ccval.type_of ccbody ]), ccparams)))
  | Ast.Block lines ->
      let resolver, cclines = List.fold_map ~init:resolver ~f:resolve lines in
      let last = List.last_exn cclines in
      (resolver, last)
  | Ast.Call { callee; args } | Ast.Binop { callee; args } ->
      let resolver, ccargs = List.fold_map ~init:resolver ~f:resolve args in
      let resolver, ccallee = resolve resolver callee in
      resolve_call resolver node ccallee ccargs
  | Ast.Var _ -> (
      match Name_resolution.Names.deref resolver.ns node with
      | Some reference -> resolve resolver reference
      | None -> failwith "unknown reference" )
  | Ast.FloatLiteral s -> (resolver, Const (Float64 (Float.of_string s)))
  | Ast.IntLiteral s -> (resolver, Const (Int64 (Int64.of_string s)))
  | Ast.StringLiteral s -> (resolver, Const (String s))
  | Ast.Return v -> resolve resolver v
  | Ast.Let (_var, value) -> resolve resolver value
  | Ast.Extern { typ; _ } ->
      let resolver, typ = resolve_type resolver typ in
      (resolver, IsInstance typ)
  | Ast.If (cond, b1, b2) ->
      let resolver, _ = resolve resolver cond in
      let resolver, b1 = resolve resolver b1 in
      let resolver, _b2 = resolve resolver b2 in
      (resolver, b1)
  | Ast.Tuple items ->
      let resolver, items = List.fold_map ~init:resolver ~f:resolve items in
      (resolver, Tuple items)
  | Ast.Param { typ; _ } -> (
      match typ with
      | Some typ ->
          let resolver, t = resolve_type resolver typ in
          (resolver, IsInstance t)
      | None -> (resolver, Unknown) )
  | Ast.Builtin Builtins.ADD_INT ->
      let a = Is Builtins.INT64 in
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [ a ]), [ a; a ])))
  | Ast.Builtin Builtins.ADD_FLOAT ->
      let a = Is Builtins.FLOAT64 in
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [ a ]), [ a; a ])))
  | Ast.Builtin Builtins.ADD
  | Ast.Builtin Builtins.SUB
  | Ast.Builtin Builtins.MUL
  | Ast.Builtin Builtins.DIV
  | Ast.Builtin Builtins.MOD ->
      let a = TypeVar 1 in
      (resolver, IsInstance (Forall (1, Appl (Appl (Is Builtins.FUNC, [ a ]), [ a; a ]))))
  | Ast.Builtin Builtins.EQ
  | Ast.Builtin Builtins.NEQ
  | Ast.Builtin Builtins.LT
  | Ast.Builtin Builtins.LTE
  | Ast.Builtin Builtins.GT
  | Ast.Builtin Builtins.GTE ->
      let a = TypeVar 1 in
      ( resolver,
        IsInstance (Forall (1, Appl (Appl (Is Builtins.FUNC, [ Is Builtins.BOOL ]), [ a; a ]))) )
  | Empty -> (resolver, Unknown)
  | Ast.Overload { name; items } ->
      let resolver, cc_items = List.fold_map ~init:resolver ~f:resolve items in
      (resolver, Ccval.Overload cc_items)
  | e ->
      Ast.nodeName e |> Stdio.printf " [TypeRes unhandled: %s]\n";
      Stdlib.flush_all ();
      ignore @@ Caml.exit 1;
      (resolver, Unknown)

(* (Memoized) Turns ast nodes into Ccvals which are the type solver's representation of type
   constraints *)
and resolve resolver (node : Ast.node) =
  match Map.find resolver.memo node with
  | Some v -> (resolver, v)
  | None -> (
      let resolver, v = _resolve resolver node in
      match v with
      | Unknown -> (resolver, v) (* don't memoize Unknowns since this adds no data *)
      | v ->
          Stdio.printf "\x1b[1;32m%25s\x1b[0m  %s\n" (Ast.nodeName (fst node)) (Ccval.show v);
          ( match Map.find resolver.overload_index node with
          | Some i -> Stdio.printf "%25s overloaded %d\n" "" i
          | None -> () );
          Stdlib.flush_all ();

          ({ resolver with memo = Map.set ~key:node ~data:v resolver.memo }, v) )

let resolve ns _ =
  Stdio.printf "------------------------------\n";
  let result, _ =
    match Name_resolution.Scope.find ~name:".init" ns.Name_resolution.Names.current_scope with
    | None -> failwith "no main func found"
    | Some main_func ->
        let init =
          {
            Resolver.ns;
            memo = Map.empty (module Ast.Node);
            instantiations = Map.empty (module Int);
            overload_index = Map.empty (module Ast.Node);
          }
        in
        resolve init main_func
  in
  ignore @@ Caml.exit 1;
  result
