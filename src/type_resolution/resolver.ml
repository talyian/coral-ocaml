(* Type Resolver 3

   In this version, we basically do compile-time evaluation, converting an Ast into a set of
   ccvalues. What's the difference between an Ast.node and a ccval? ccvals have types *)

(* avoid being shadowed by Coral_core.Types *)
module Local_types = Types
open Coral_core
open Base
open Local_types

type ccval = Ccval.t
type cconst = Cconst.t

(** A resolver is the data for a type resolution compilation pass. It follows name resolution (and
    relies on Name resolver ) and produces a map of Ast.Node -> ccval for later stages *)
module Resolver = struct
  type t =
    { ns: Names.t
    ; memo: ccval Ast.Node.Map.t
    ; instantiations: ccval Map.M(Int).t
    ; (* For any given call node referencing an overload, stores the index of which overload was
         resolved *)
      overload_index: int Ast.Node.Map.t
    ; freevars: int }

  let create ns =
    { ns
    ; memo= Map.empty (module Ast.Node)
    ; overload_index= Map.empty (module Ast.Node)
    ; instantiations= Map.empty (module Int)
    ; freevars= 0 }

  (* A child can reference elements from the parent but not vice versa *)
  let create_child parent =
    let x = create parent.ns in
    {x with freevars= parent.freevars + 100}

  let dump resolver =
    Map.iteri resolver.memo ~f:(fun ~key ~data ->
        Stdio.printf "      [%20s] => %s\n" (Ast.nodeName (fst key)) (Ccval.show data))

  let with_mapping (_id : int) _ccval res = res

  let with_overload node idx res =
    {res with overload_index= Map.set ~key:node ~data:idx res.overload_index}

  (** Adds a type value for a given Ast node *)
  let add node data resolver = {resolver with memo= Map.set ~key:node ~data resolver.memo}

  (** Adds a free type term for a given Ast node *)
  let add_free_no_expr resolver =
    let i = resolver.freevars in
    ({resolver with freevars= i + 1}, Ccval.Freevar i)

  let add_free node resolver =
    let i = resolver.freevars in
    let resolver =
      {resolver with memo= Map.set ~key:node ~data:(Freevar i) resolver.memo; freevars= i + 1}
    in
    (resolver, Ccval.Freevar i)

  (** recursively replaces all occurrences of a type value with another *)
  let subst ~key ~repl resolver =
    let memo = resolver.memo in
    let memo = Map.map ~f:(Ccval.subst ~key ~repl) memo in
    {resolver with memo}
end

type resolver = Resolver.t

let get (resolver : resolver) node = Map.find resolver.memo node

let rec resolve_call (resolver : resolver) call callee (args : ccval list) =
  let open Ccval in
  match callee with
  | IsInstance (Appl (Appl (Is Builtins.VARFUNC, ret), _)) ->
      (resolver, is_instance @@ make_tuple_from_return ret)
  | IsInstance (Appl (Appl (Is Builtins.FUNC, ret), params)) ->
      let resolver =
        List.fold2_exn ~init:resolver
          ~f:(fun resolver param arg ->
            let param_type = param in
            let arg_type = Ccval.type_of arg in
            if Ccval.equal param_type arg_type then resolver
            else
              (* unify params and args. If no exc is thrown then the call succeeds *)
              match (param_type, arg_type) with
              | ptype, Freevar i ->
                  Stdio.printf "instantiating free variable %d with %s\n" i (Ccval.show param) ;
                  Resolver.subst ~key:arg_type ~repl:ptype resolver
              | _ ->
                  failwith
                  @@ Printf.sprintf "value mismatch %s <-> %s" (Ccval.show param) (Ccval.show arg))
          params args in
      (resolver, is_instance @@ make_tuple_from_return ret)
  | Overload items ->
      (* When calling an overload set, try all the cases until one fits *)
      (* tag the call expr with the overload index as well *)
      let rec resolve_overload res i = function
        | [] ->
            Stdio.printf "Overload resolution failed for %s\n" (Ast.show_node call) ;
            (res, Unknown) (* no overload matched *)
        | x :: xs -> (
          match resolve_call res call x args with
          | exception _ -> resolve_overload res (i + 1) xs
          | res, ret_val ->
              Stdio.printf "Overload success for %s at %d\n" (Ast.show_node call) i ;
              let res = Resolver.with_overload call i res in
              (res, ret_val) ) in
      resolve_overload resolver 0 items
  | IsInstance (Forall (_t, Appl (Appl (Is Builtins.FUNC, _ret), params))) ->
      (* if a call is to a universally quantified type, we need to unify it against the arguments.
         We need to instantiate the quantified variable *)
      Stdio.printf "unk\n" ;
      let tvar = Ccval.Unknown in
      let resolver = Resolver.with_mapping _t tvar resolver in
      let resolver =
        List.fold2_exn
          ~f:(fun res arg param ->
            Stdio.printf "---------- unifying call: %s : %s\n" (Ccval.show arg) (Ccval.show param) ;
            (* TODO: universal quantification *)
            (* arg param *)
            res)
          ~init:resolver args params in
      (* let env = Environment.(empty |> add_var _t) in *)
      (* let env, out = Environment.unify_call env params args ret in *)
      Stdio.printf "unk\n" ;
      let out = Ccval.Unknown in
      (resolver, out)
  | _ ->
      failwith
      @@ Printf.sprintf "unknown call situation %s (%s)\n" (Ccval.show callee)
           (String.concat ~sep:", " @@ List.map ~f:Ccval.show args)

exception Ellipsis

let rec resolve_type (resolver : resolver) typ =
  let open Ccval in
  match typ with
  (* TODO: these should be picked via name resolution *)
  | Type.Name name ->
      Names.deref_type resolver.ns typ
      |> (fun x -> Option.value_exn ~message:("unknown type " ^ name) x)
      |> resolve resolver
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

and _resolve resolver node =
  let open Ccval in
  match fst node with
  | Ast.Func {params; body; name; ret_type} ->
      (* TODO: we really should be doing something particular with return x instead of just
         evaluating it the same as x *)
      Stdio.printf "resolving function: %s\n" name ;
      (* let resolver = Resolver.create_child resolver in *)
      (* let freevars_begin = resolver.Resolver.freevars in *)
      let resolver, ccparams = List.fold_map ~init:resolver ~f:resolve params in
      let ccparams = List.map ~f:Ccval.type_of ccparams in
      let resolver, cc_ret_type =
        match ret_type with
        | None -> Resolver.add_free_no_expr resolver
        | Some t -> resolve_type resolver t in
      (* initially set the function type to its declared type *)
      let cc_functype = Appl (Appl (Is Builtins.FUNC, [cc_ret_type]), ccparams) in
      let resolver = Resolver.add node cc_functype resolver in
      let resolver, ccbody = resolve resolver body in
      (* find all the returns in the function and unify them with cc_ret_type *)
      let resolver =
        let f ~key ~data resolver =
          if Ast.Node.( = ) data node then (
            let cc_ret_type = Map.find_exn resolver.Resolver.memo key in
            let cc_ret_type = Ccval.type_of cc_ret_type in
            let cc_functype = Appl (Appl (Is Builtins.FUNC, [cc_ret_type]), ccparams) in
            Stdio.printf "%s - %s\n" (Ast.nodeName @@ fst key) (Ast.nodeName @@ fst data) ;
            Resolver.add node cc_functype resolver )
          else resolver in
        Map.fold resolver.ns.returns ~init:resolver ~f in
      (* if resolver.Resolver.freevars <> freevars_begin then (
       *   Stdio.printf "function call produced free variables: %s\n" name ;
       *   Resolver.dump resolver ;
       *   Caml.exit 1 ) *)
      (* else Stdio.printf "function call: %s (%d free vars)\n" name freevars_begin ; *)
      Stdio.printf "------------------------------\n" ;
      Resolver.dump resolver ;
      Stdlib.flush_all () ;
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [Ccval.type_of ccbody]), ccparams)))
  | Ast.Block lines ->
      let resolver, cclines = List.fold_map ~init:resolver ~f:resolve lines in
      let last = List.last_exn cclines in
      (resolver, last)
  | Ast.Call {callee; args} | Ast.Binop {callee; args} ->
      let resolver, ccargs = List.fold_map ~init:resolver ~f:resolve args in
      let resolver, ccallee = resolve resolver callee in
      let resolver, return_type = resolve_call resolver node ccallee ccargs in
      let resolver = Resolver.add node return_type resolver in
      (resolver, return_type)
  | Ast.Var _ -> (
    match Names.deref resolver.ns node with
    | Some reference -> resolve resolver reference
    | None -> failwith "unknown reference" )
  | Ast.FloatLiteral s -> (resolver, Const (Float64 (Float.of_string s)))
  | Ast.IntLiteral s -> (resolver, Const (Int64 (Int64.of_string s)))
  | Ast.StringLiteral s -> (resolver, Const (String s))
  | Ast.Return v ->
      let resolver, v = resolve resolver v in
      (resolver, v)
  | Ast.Let (_var, value) -> resolve resolver value
  | Ast.Extern {typ; _} ->
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
  | Ast.Param {typ; _} -> (
    match typ with
    | Some typ ->
        let resolver, t = resolve_type resolver typ in
        (resolver, IsInstance t)
    | None ->
        let resolver, ty = Resolver.add_free node resolver in
        (resolver, IsInstance ty) )
  | Ast.Builtin Builtins.SUB_INT | Ast.Builtin Builtins.MOD_INT | Ast.Builtin Builtins.ADD_INT ->
      let a = Is Builtins.INT64 in
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [a]), [a; a])))
  | Ast.Builtin Builtins.ADD_FLOAT ->
      let a = Is Builtins.FLOAT64 in
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [a]), [a; a])))
  | Ast.Builtin Builtins.ADD_STR ->
      let a = Is Builtins.STR in
      (resolver, IsInstance (Appl (Appl (Is Builtins.FUNC, [a]), [a; a])))
  | Ast.Builtin Builtins.ADD
   |Ast.Builtin Builtins.SUB
   |Ast.Builtin Builtins.MUL
   |Ast.Builtin Builtins.DIV
   |Ast.Builtin Builtins.MOD ->
      let a = TypeVar 1 in
      (resolver, IsInstance (Forall (1, Appl (Appl (Is Builtins.FUNC, [a]), [a; a]))))
  | Ast.Builtin Builtins.EQ_INT
   |Ast.Builtin Builtins.EQ_FLOAT
   |Ast.Builtin Builtins.EQ
   |Ast.Builtin Builtins.NEQ
   |Ast.Builtin Builtins.LT
   |Ast.Builtin Builtins.LTE
   |Ast.Builtin Builtins.GT
   |Ast.Builtin Builtins.GTE ->
      let a = TypeVar 1 in
      ( resolver
      , IsInstance (Forall (1, Appl (Appl (Is Builtins.FUNC, [Is Builtins.BOOL]), [a; a]))) )
  | Ast.Builtin Builtins.ELLIPSIS -> raise Ellipsis
  | Ast.Builtin builtin -> (resolver, Ccval.Is builtin)
  | Empty -> (resolver, Unknown)
  | Ast.Overload {name= _; items} ->
      let resolver, cc_items = List.fold_map ~init:resolver ~f:resolve items in
      (resolver, Ccval.Overload cc_items)
  | e ->
      Ast.nodeName e |> Stdio.printf " [TypeRes unhandled: %s]\n" ;
      Stdlib.flush_all () ;
      ignore @@ Caml.exit 1 ;
      (resolver, Unknown)

(* (Memoized) Turns ast nodes into Ccvals which are the type solver's representation of type
   constraints *)
and resolve resolver (node : Ast.node) =
  match Map.find resolver.memo node with
  | Some v -> (resolver, v)
  | None -> (
      let resolver, v = _resolve resolver node in
      match v with
      (* | Unknown -> (resolver, v) (\* don't memoize Unknowns since this adds no data *\) *)
      | v ->
          Stdio.printf "\x1b[1;32m%25s\x1b[0m  %s\n" (Ast.nodeName (fst node)) (Ccval.show v) ;
          ( match Map.find resolver.overload_index node with
          | Some i -> Stdio.printf "%25s overloaded %d\n" "" i
          | None -> () ) ;
          Stdlib.flush_all () ;
          ({resolver with memo= Map.set ~key:node ~data:v resolver.memo}, v) )

let resolve ns _ =
  Stdio.printf "------------------------------\n" ;
  let result, _ =
    match Names.Scope.find ~name:".init" ns.Names.current_scope with
    | None -> failwith "no main func found"
    | Some main_func ->
        let init = Resolver.create ns in
        resolve init main_func in
  Map.iteri result.overload_index ~f:(fun ~key ~data ->
      Stdio.printf " overload: %s == %d\n" (Ast.nodeName @@ fst key) data) ;
  result
