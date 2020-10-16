(* Type Resolver 3

   In this version, we basically do compile-time evaluation, converting an Ast into a set of ccvalues. What's the difference between an Ast.node and a ccval? ccvals have types *)
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

type ccval =
  | Error (* this represents a type error, always false *)
  | Unknown (* this represents a unknown variable, always true *)
  | Const of Cconst.t

  | Forall of int * ccval (* a universal quantifier - introduces a scope for a type variable *)
  | TypeVar of int (* a free type variable introduced within the scope of a universal quantifier *)

  | Is of Builtins.t
  | Appl of ccval * ccval list
  | IsInstance of ccval
  | TypeOf of ccval

  | Call of ccval * ccval list
  | Tuple of ccval list
[@@deriving equal]
  (* | Lambda of {
   *     params: ccval list;
   *     openvars: ccval list;
   *     body: ccval
   *   } *)
  (* | Block of ccval list *)
let rec show_ccval v =
  let cc x = String.concat ~sep:", " @@ List.map ~f:show_ccval x in
  match v with
  | Error -> "err"
  | Unknown -> "???"
  | Const c -> Cconst.show c
  | Forall (x, body) -> Printf.sprintf "∀.%d[%s]" x (show_ccval body)
  | TypeVar x -> Printf.sprintf "τ%d" x
  | IsInstance (Is Builtins.VOID) -> "()"
  | Is Builtins.VOID -> "void"
  | Is b -> Builtins.show b
  | Appl (x, args) -> Printf.sprintf "%s[%s]" (show_ccval x) (cc args)
  | Call (x, args) -> Printf.sprintf "%s(%s)" (show_ccval x) (cc args)
  | IsInstance x -> "::" ^ show_ccval x
  | TypeOf x -> "@" ^ show_ccval x
  | Tuple x -> "(" ^ (cc x) ^ ")"
  (* | Lambda {params; openvars=_; body} -> Printf.sprintf "λ.%s(%s)" (cc params) (show_ccval body) *)

type resolver =  {
  ns: Name_resolution.Names.t;
  memo: ccval Ast.Node.Map.t
}

(** An Environment is a set of free and bound variables *)
module Environment = struct
  type t = { foo: int }

  let empty = { foo = 1 }

  let add_var var t = t

  let bind key repl t = t, repl


  let eval_in env x = x

  let rec unify env (x:ccval) (y:ccval) =
    if equal_ccval x y then
      env, x
    else _unify env x y
  and _unify env x y =
    match x, y  with
    | TypeVar x, TypeVar y ->
      bind (TypeVar x) (TypeVar y) env
    | TypeVar x, y ->
      bind (TypeVar x) y env
    | x, TypeVar y ->
      bind (TypeVar y) x env
    | x, y ->
      failwith @@ Printf.sprintf "cannot unify %s and %s" (show_ccval x) (show_ccval y)

  let unify_call (environment:t) params args ret =
    let rec unifyarg (env:t) = function
      | [], [] -> env
      | x :: xs, y :: ys ->
        let env, z = unify env x y in unifyarg env (xs, ys)
      | _ -> failwith "unify: argument count mismatch" in
    let env = unifyarg environment (params, args) in
    let ret = eval_in env ret in
    env, List.nth_exn ret 0

end
module Ccval = struct
  let type_of = function
    | Tuple [] -> Is Builtins.VOID
    | IsInstance x -> x
    | Const(Cconst.Bool _) -> Is Builtins.BOOL
    | Const(Cconst.Int8 _) -> Is Builtins.INT8
    | Const(Cconst.Int32 _) -> Is Builtins.INT32
    | Const(Cconst.Int64 _) -> Is Builtins.INT64
    | Const(Cconst.Float64 _) -> Is Builtins.FLOAT64
    | Const(Cconst.String _) -> Is Builtins.STR
    | x -> TypeOf x

  let rec is_instance = function
    | TypeOf x -> x
    | Tuple ts -> Tuple (List.map ~f:is_instance ts)
    | x -> IsInstance x

  let make_tuple = function
    | [] -> (Is Builtins.VOID)
    | [x] -> x
    | xs -> Tuple xs

  let call data callee args =
    match callee with
    | IsInstance(Appl(Appl(Is Builtins.VARFUNC, ret), _)) ->
      data, is_instance @@ make_tuple ret
    | IsInstance(Appl(Appl(Is Builtins.FUNC, ret), _)) ->
      data, is_instance @@ make_tuple ret
    | IsInstance(Forall(_t, Appl(Appl(Is Builtins.FUNC, ret), params))) ->
      let env = Environment.(empty |> add_var _t) in
      let env, out = Environment.unify_call env params args ret in
      data, out
    | _ ->
      Stdio.eprintf "unknown call situation %s (%s)\n"
        (show_ccval callee)
        (String.concat ~sep:", " @@ List.map ~f:show_ccval args);
      failwith "unknown call"
end

exception Ellipsis
let rec resolve_type data typ =
  match typ with
  | Type.Name "Int64" -> data, Is Builtins.INT64
  | Type.Name "Func" -> data, Is Builtins.FUNC
  | Type.Name "Str" -> data, Is Builtins.STR
  | Type.Name "..." -> raise Ellipsis
  | Type.Parameterized(Type.Parameterized (Type.Name "Func", ret), params) ->
    (try
      let data, bs = List.fold_map ~f:resolve_type ~init:data params in
      let data, cs = List.fold_map ~f:resolve_type ~init:data ret in
      let data, a = resolve_type data (Type.Name "Func") in
      data, Appl(Appl (a, cs), bs)
     with Ellipsis ->
       let params = List.filter ~f:(function | Type.Name "..." -> false | _ -> true) params in
       let data, bs = List.fold_map ~f:resolve_type ~init:data params in
       let data, cs = List.fold_map ~f:resolve_type ~init:data ret in
       let a = Is Builtins.VARFUNC in
       data, Appl(Appl (a, cs), bs))
  | Type.Parameterized (a, bs) ->
    let data, bs = List.fold_map ~f:resolve_type ~init:data bs in
    let data, a = resolve_type data a in
    data, Appl (a, bs)
  | t -> failwith @@ "unknown type" ^ (Type.show Ast.pp_node t)

let rec _resolve data node =
  match fst node with
  | Ast.Func {params; body; _} ->
    (* TODO: we really should be doing something particular with return x
       instead of just evaluating it the same as x *)
    let data, ccparams = List.fold_map ~init:data ~f:resolve params in
    let ccparams = List.map ~f:Ccval.type_of ccparams in
    (* to break infinite recursion, we need to add a term here *)
    let data = {data with memo = Map.set ~key:node ~data:Unknown data.memo} in
    let data, ccbody = resolve data body in
    data, IsInstance(Appl(Appl(Is Builtins.FUNC, [Ccval.type_of ccbody]), ccparams))
  | Ast.Block lines ->
    let data, cclines = List.fold_map ~init:data ~f:resolve lines in
    let last = List.last_exn cclines in
    data, last
  | Ast.Call {callee; args}
  | Ast.Binop { callee; args} ->
    let data, ccargs = List.fold_map ~init:data ~f:resolve args in
    let data, ccallee = resolve data callee in
    Ccval.call data ccallee ccargs
  | Ast.Var _ ->
    ( match Name_resolution.Names.deref data.ns node with
    | Some reference -> resolve data reference
    | None -> failwith "unknown reference")
  | Ast.IntLiteral s -> data, Const(Int64 (Int64.of_string s))
  | Ast.StringLiteral s -> data, Const(String s)
  | Ast.Return v -> resolve data v
  | Ast.Let (_var, value) -> resolve data value
  | Ast.Extern {typ;_} -> let data, typ = resolve_type data typ in data, IsInstance typ
  | Ast.If (cond, b1, b2) ->
    let data, _ = resolve data cond in
    let data, b1 = resolve data b1 in
    let data, _b2 = resolve data b2 in
    data, b1
  | Ast.Tuple items ->
    let data, items = List.fold_map ~init:data ~f:resolve items in
    data, Tuple items
  | Ast.Param {typ;_} -> (match typ with
      | Some typ ->
        let data, t = resolve_type data typ in
        data, IsInstance t
      | None -> data, Unknown)
  | Ast.Builtin Builtins.ADD
  | Ast.Builtin Builtins.SUB
  | Ast.Builtin Builtins.MUL
  | Ast.Builtin Builtins.DIV
  | Ast.Builtin Builtins.MOD ->
    let a = TypeVar 1 in
    data, IsInstance(Forall(1, Appl(Appl(Is Builtins.FUNC, [a]), [a; a])))
  | Ast.Builtin Builtins.EQ
  | Ast.Builtin Builtins.NEQ
  | Ast.Builtin Builtins.LT
  | Ast.Builtin Builtins.LTE
  | Ast.Builtin Builtins.GT
  | Ast.Builtin Builtins.GTE ->
    let a = TypeVar 1 in
    data, IsInstance(Forall(1, Appl(Appl(Is Builtins.FUNC, [Is Builtins.BOOL]), [a; a])))
  | Empty -> data, Unknown
  | e ->
    Ast.nodeName e |> Stdio.printf " [typeres: %s]\n";
    Stdlib.flush_all ();
    ignore @@ Caml.exit 1;
    data, Unknown

(* (Memoized) Turns ast nodes into Ccvals which are the type solver's
   representation of type constraints *)
and resolve data (node:Ast.node) =
  match Map.find data.memo node with
  | Some v -> data, v
  | None ->
    let data, v = _resolve data node in
    match v with
    | Unknown -> data, v
    | v ->
      Stdio.printf "\x1b[1;32m%25s\x1b[0m  %s\n" (Ast.nodeName (fst node)) (show_ccval v);
      Stdlib.flush_all ();
      {data with memo = Map.set ~key:node ~data:v data.memo} , v

let resolve ns _ =
  Stdio.printf "------------------------------\n";
  let result, _ =
    match Name_resolution.Scope.find ~name:".init" ns.Name_resolution.Names.current_scope with
    | None -> failwith "no main func found"
    | Some main_func -> resolve {ns; memo = Map.empty (module Ast.Node)} main_func
  in
  ignore @@ Caml.exit 1;
  result
