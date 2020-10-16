open Base

(*
   The main type here is node.
   Before that, here are all the variant types for node: these are all parameterized with 'a = node.
   Why I did this instead of making them all recursive, I don't remember.
*)
type 'a coraltype = 'a Type.t [@@deriving show, sexp_of, compare]

type 'a varInfo = { name : string; varType : 'a coraltype option }
[@@deriving sexp_of, compare]

(* we customize the impl for show_varInfo just to make it slightly less verbose *)
let pp_varInfo (f : Formatter.t -> 'a -> unit) (fmt : Formatter.t)
    (v : 'a varInfo) =
  Stdlib.Format.fprintf fmt "%s" v.name;
  match v.varType with None -> () | Some t -> pp_coraltype f fmt t

type 'a tupleInfo = { name : string; fields : (string * 'a coraltype) list }
[@@deriving show, sexp_of, compare]

type 'a memberInfo = { base : 'a; memberName : string }
[@@deriving show, sexp_of, compare]

module Info = struct
  type t = { id : Id.t } [@@deriving show, sexp_of]

  let create () = { id = Id.next () }

  let compare a b = compare a.id b.id
end

type node_data =
  | Module of { name : string; lines : node list }
  | Import of {
      path : string list;
      names :
        [ `Module of string option | `All | `Member of string * string option ]
        list;
    }
  | Extern of { binding : string; name : string; typ : node Type.t }
  | Func of {
      name : string;
      ret_type : node coraltype option;
      params : node list;
      body : node;
    }
  | Param of { idx: int; name : string; typ : node coraltype option }
  | Comment of string
  | Binop of { callee : node; args : node list }
  | Call of { callee : node; args : node list }
  | If of (node * node * node)
  | IntLiteral of string
  | FloatLiteral of string
  | CharLiteral of char
  | StringLiteral of string
  | Var of node varInfo
  | Let of node varInfo * node
  | Set of node varInfo * node
  | Block of node list
  | Tuple of node list
  | List of node list
  | TupleDef of node tupleInfo
  | Member of node memberInfo
  | Return of node
  | Builtin of Builtins.t
  | Empty
[@@deriving show, sexp_of]

and node = node_data * Info.t [@@deriving show, sexp_of]

let compare_node (a : node) (b : node) = compare (snd a).Info.id (snd b).Info.id

module Node = struct
  module T = struct
    type t = node [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make (T)

  module Map = struct
    type 'v t = (T.t, 'v, comparator_witness) Map.t
  end
end

let rec nodeName = function
  | Module _ -> "Module"
  | Import {path;_} -> "Import-" ^ (String.concat ~sep:"." path)
  | Extern {name;_} -> "Extern-" ^ name
  | Func {name;_} -> "Func-" ^ name
  | Comment _ -> "Comment"
  | If _ -> "If"
  | IntLiteral _ -> "IntLiteral"
  | FloatLiteral _ -> "FloatLiteral"
  | StringLiteral _ -> "StringLiteral"
  | Var v -> "Var-" ^ v.name
  | Block _ -> "Block"
  | Call {callee=(Var v, _);_ } -> "Call-" ^ v.name
  | Call {callee;_} -> "Call-" ^ nodeName (fst callee)
  | Tuple _ -> "Tuple"
  | Return x -> "return-" ^ nodeName (fst x)
  | Empty -> "Empty"
  | Binop {callee=(Var v, _);_} -> "Binop-" ^ v.name
  | Binop {callee;_} -> "Binop-" ^ nodeName(fst callee)
  | Let (v, _) -> "Let-" ^ v.name
  | Set (v, _) -> "Set-" ^ v.name
  | Member _ -> "Member"
  | TupleDef _ -> "TupleDef"
  | List _ -> "List"
  | CharLiteral _ -> "CharLiteral"
  | Builtin b -> Builtins.show b
  | Param {name;_} -> "Param-" ^ name

let mm foo = (foo, Info.create ())

(* some simplified constructors for ast nodes. I question whether these are really of any use *)
module Make = struct
  let binop (op_name, lhs, rhs) =
    let op = { name = op_name; varType = None } in
    mm @@ Binop { callee = mm @@ Var op; args = [ lhs; rhs ] }

  let callNode callee args = mm @@ Call { callee; args }

  let funcNode (name, ret_type, params, body) =
    mm @@ Func { name; ret_type; params; body }

  let moduleNode lines = mm @@ Module { name = "module"; lines }

  let extern binding name typ = mm @@ Extern { binding; name; typ }

  let rec ifNode cond ifbody elifs elsebody =
    match elifs with
    | [] ->
      mm @@ If (cond, ifbody, elsebody)
    | (elcond, elbody) :: elif_rest ->
      mm @@ If (cond, ifbody, ifNode elcond elbody elif_rest elsebody)
end

let fold ~init ~f (node : node) =
  let e, _ = node in
  match e with
  | Module { lines; _ } -> List.fold ~init ~f lines
  | Block lines -> List.fold ~init ~f lines
  | Tuple lines -> List.fold ~init ~f lines
  | List lines -> List.fold ~init ~f lines
  | Binop { callee; args } | Call { callee; args } ->
      let init = f init callee in
      List.fold ~init ~f args
  | Func { params; body; _ } ->
      let init = List.fold ~init ~f params in
      f init body
  | If (a, b, c) ->
      let init = f init a in
      let init = f init b in
      f init c
  | Let (_, v) -> f init v
  | Set (_, v) -> f init v
  | Member { base = v; _ } -> f init v
  | Return v -> f init v
  | CharLiteral _ | StringLiteral _ | IntLiteral _ | FloatLiteral _ | Comment _
  | Import _ | Extern _ | Builtin _ | Var _ ->
      init
  | TupleDef _ -> init
  | Empty -> init
  | Param _ -> init

let iter (f : node -> unit) e = fold ~init:() ~f:(fun () -> f) e
