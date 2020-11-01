open Base
open Coral_core

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
    | Freevar of int
  [@@deriving equal]

  let fold ~init ~f e =
    let init = f init e in
    match e with
    | Overload items -> List.fold ~init ~f items
    | Appl (a, items) -> List.fold ~init:(f init a) ~f items
    | Call (a, items) -> List.fold ~init:(f init a) ~f items
    | Tuple inner -> List.fold ~init ~f inner
    | And (a, b) -> f (f init a) b
    | Forall (_, inner) -> f init inner
    | IsInstance inner -> f init inner
    | TypeOf inner -> f init inner
    | Error | Unknown | Const _ | TypeVar _ | Is _ -> init
    | OverloadIndex _ -> init
    | Freevar _ -> init

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
    | Freevar i -> "\x1b[1;33mτ\x1b[0m" ^ Int.to_string i

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

  let make_tuple_from_return = function [] -> Is Builtins.VOID | [ x ] -> x | xs -> Tuple xs

  let subst ~key ~repl e =
    if equal e key then repl
    else
      match e with
      | Error | Unknown | Overload _ | Const _
      | Forall (_, _)
      | TypeVar _ | Is _
      | Appl (_, _)
      | IsInstance _ | TypeOf _
      | Call (_, _)
      | Tuple _
      | And (_, _)
      | OverloadIndex _ | Freevar _ ->
          Stdio.printf "subst %s (%s) oops\n" (show e) (show key);
          failwith "oops"
end
