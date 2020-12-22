(* Ast Node - these represent an "expression"
 * They are created in the parser and can be tagged with additional information in each pass.
 * a pass can naively store data in a Map<node, value>
 * but could alternatively foldmap over an Ast producing a new one
 *)

open Base

(* Represents a thing you import:
 * import foo.bar.baz
 * import foo.bar.baz (...)
 * import foo.bar.baz (alpha, Bravo as beta) 
 *)
type importType = Module of string option | All | Member of string * string option
[@@deriving compare, sexp]

module Type = struct
  type 'node t =
    | Name of string
    | Dotted of {base: 'node t; member: string}
    | Applied of {base: 'node t; params: 'node t list}
    | Ref of 'node
    | Ellipsis
  [@@deriving sexp, compare]
end

(* An Ast can be decorated with additional info per node.
 * name resolution can point to other ast nodes
 * type resolution can store a set of type constraints as well as a primary type
 * effect resolution - IO / exceptions
 * llvm codegen - stores a llvmtype/llvoid/llvmvalue *)
module type Info_S = sig
  type t [@@deriving compare, sexp]
end

(* module Adt (Info : Info_S) = struct *)
module Adt = struct
  module Info = struct
    type t = {id: int} [@@deriving compare, sexp]

    let _id = ref 0

    let create () =
      _id := !_id + 1 ;
      {id= !_id}
  end

  type info = Info.t [@@deriving sexp, compare]

  module Node = struct
    module Node0 = struct
      type 'info t0 =
        | Module of {name: string; lines: node list; info: info}
        | Call of {callee: node; args: node list; info: info}
        | Import of {path: string list; names: importType list; info: info}
        | Extern of {binding: string; name: string; typ: coraltype; info: info}
        | Func of
            {name: string; ret_type: coraltype option; params: node list; body: node; info: info}
        | Param of {idx: int; name: string; typ: coraltype option; info: info}
        | Comment of {comment: string; info: info}
        | Binop of {callee: node; args: node list; info: info}
        | If of {cond: node; ifbody: node; elsebody: node; info: info}
        | IntLiteral of {literal: string; value: int64; info: info}
        | FloatLiteral of {literal: string; value: float; info: info}
        | CharLiteral of {literal: string; info: info}
        | StringLiteral of {literal: string; info: info}
        | Var of {name: string; info: info}
        | Let of {name: string; typ: coraltype option; value: node; info: info}
        | Set of {name: node; value: node; info: info}
        | Block of {items: node list; info: info}
        | Tuple of {items: node list; info: info}
        | List of {items: node list; info: info}
        | Member of {base: node; member: string; info: info}
        | Return of {value: node; info: info}
        | Builtin of {builtin: Builtins.t; info: info}
        | Overload of {name: string; items: node list; info: info}
        | Empty of {info: info}
        | Type of {typ: coraltype; info: info}
      [@@deriving compare, sexp]

      and t = info t0

      and node = t

      and coraltype = node Type.t
    end

    include Node0
    include Comparable.Make (Node0)
  end

  type node = Node.t [@@deriving compare, sexp]
  type coraltype = Node.coraltype [@@deriving sexp, compare]

  (** preorder fold *)
  let fold_info ~init ~f e =
    let init = f init e in
    match e with
    | Node.Module {lines; _} -> List.fold ~init ~f lines
    | Call {callee; args; _} | Binop {callee; args; _} ->
        f init callee |> fun init -> List.fold ~init ~f args
    | If {cond; ifbody; elsebody; _} -> f (f (f init cond) ifbody) elsebody
    | Func {body; _} -> f init body
    | Let {value; _} -> f init value
    | Set {value; _} -> f init value
    | Block {items; _} | Tuple {items; _} | List {items; _} -> List.fold ~init ~f items
    | Member _ | Return _ | Import _ | Extern _ | Param _ | Comment _ | IntLiteral _ | Type _
     |FloatLiteral _ | CharLiteral _ | StringLiteral _ | Var _ | Builtin _ | Overload _ | Empty _
      ->
        init

  let map_info ~(f : node -> Info.t -> Info.t) e =
    match e with
    | Node.Module m -> Node.Module {m with info= f e m.info}
    | Node.Call m -> Node.Call {m with info= f e m.info}
    | Import m -> Import {m with info= f e m.info}
    | Extern m -> Extern {m with info= f e m.info}
    | Func m -> Func {m with info= f e m.info}
    | Param m -> Param {m with info= f e m.info}
    | Comment m -> Comment {m with info= f e m.info}
    | Binop m -> Binop {m with info= f e m.info}
    | If m -> If {m with info= f e m.info}
    | IntLiteral m -> IntLiteral {m with info= f e m.info}
    | FloatLiteral m -> FloatLiteral {m with info= f e m.info}
    | CharLiteral m -> CharLiteral {m with info= f e m.info}
    | StringLiteral m -> StringLiteral {m with info= f e m.info}
    | Var m -> Var {m with info= f e m.info}
    | Let m -> Let {m with info= f e m.info}
    | Set m -> Set {m with info= f e m.info}
    | Block m -> Block {m with info= f e m.info}
    | Tuple m -> Tuple {m with info= f e m.info}
    | List m -> List {m with info= f e m.info}
    | Member m -> Member {m with info= f e m.info}
    | Return m -> Return {m with info= f e m.info}
    | Builtin m -> Builtin {m with info= f e m.info}
    | Overload m -> Overload {m with info= f e m.info}
    | Empty m -> Empty {info= f e m.info}
    | Type m -> Type {m with info= f e m.info}

  let get_info = function
    | Node.Module {info; _} -> info
    | Call {info; _} -> info
    | Import {info; _} -> info
    | Extern {info; _} -> info
    | Func {info; _} -> info
    | Param {info; _} -> info
    | Comment {info; _} -> info
    | Binop {info; _} -> info
    | If {info; _} -> info
    | IntLiteral {info; _} -> info
    | FloatLiteral {info; _} -> info
    | CharLiteral {info; _} -> info
    | StringLiteral {info; _} -> info
    | Var {info; _} -> info
    | Let {info; _} -> info
    | Set {info; _} -> info
    | Block {info; _} -> info
    | Tuple {info; _} -> info
    | List {info; _} -> info
    | Member {info; _} -> info
    | Return {info; _} -> info
    | Builtin {info; _} -> info
    | Overload {info; _} -> info
    | Empty {info; _} -> info
    | Type {info; _} -> info
end

(* A set of constructors for the Ast nodes. *)
module Make = struct
  open Adt.Node
  open Adt

  let moduleNode name lines = Module {name; lines; info= Info.create ()}
  let extern binding name typ = Extern {binding; name; typ; info= Info.create ()}
  let import path names = Import {path; names; info= Info.create ()}
  let var name = Var {name; info= Info.create ()}
  let returnNode value = Return {value; info= Info.create ()}
  let tuple items = Tuple {items; info= Info.create ()}
  let list items = List {items; info= Info.create ()}
  let block items = Block {items; info= Info.create ()}
  let letNode name typ value = Let {name; typ; value; info= Info.create ()}
  let setNode name value = Set {name; value; info= Info.create ()}
  let ifNodeBase cond ifbody elsebody = If {cond; ifbody; elsebody; info= Info.create ()}

  let rec ifNode cond ifbody eliflist elsebody =
    match eliflist with
    | (elif_cond, elif_body) :: rest ->
        ifNodeBase cond ifbody (ifNode elif_cond elif_body rest elsebody)
    | [] -> ifNodeBase cond ifbody elsebody

  let funcNode name ret_type params body = Func {name; ret_type; params; body; info= Info.create ()}
  let callNode callee args = Call {callee; args; info= Info.create ()}
  let binop (a, b, c) = Call {callee= var a; args= [b; c]; info= Info.create ()}
  let int_literal s = IntLiteral {literal= s; value= Int64.of_string s; info= Info.create ()}
  let float_literal s = FloatLiteral {literal= s; value= Float.of_string s; info= Info.create ()}
  let char_literal s = CharLiteral {literal= String.of_char s; info= Info.create ()}
  let string_literal s = StringLiteral {literal= s; info= Info.create ()}
  let empty = Empty {info= Info.create ()}
  let member base member = Member {base; member; info= Info.create ()}
  let param idx name typ = Param {idx; name; typ; info= Info.create ()}
end
