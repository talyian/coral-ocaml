(* Ast Node - these represent an "expression"
 * They are created in the parser and can be tagged with additional information in each pass.
 * a pass can naively store data in a Map<node, value>
 * but could alternatively foldmap over an Ast producing a new one
 *)

open Base

(* Represents a thing you import:
 * import foo.bar.baz
 * import foo.bar.baz as baz123
 * import foo.bar.baz (...)
 * import foo.bar.baz (alpha, Bravo as beta)
 *)
type importType = Module of string option | All | ImpMember of string * string option
[@@deriving compare, sexp, show]

(* module Info = struct
 *   type t = {id: int} [@@deriving compare, sexp, show {with_path= false}]
 *
 *   let _id = ref 0
 *
 *   let create () =
 *     _id := !_id + 1 ;
 *     {id= !_id}
 * end *)

module Info = struct
  type t = unit [@@deriving compare, sexp, show {with_path= false}]

  let _id = ref 0
  let create () = ()
end

module Node = struct
  type info = Info.t [@@deriving compare, sexp, show {with_path= false}]

  module Node0 = struct
    type 'info t0 =
      | Module of {name: string; lines: t list; info: info}
      | Call of {callee: t; args: t list; info: info}
      | Import of {path: string list; names: importType list; info: info}
      | Extern of {binding: string; name: string; typ: t; info: info}
      | Func of {name: string; ret_type: t option; params: t list; body: t; info: info}
      | Param of {idx: int; name: string; typ: t option; info: info}
      | Comment of {comment: string; info: info}
      | Binop of {callee: t; args: t list; info: info}
      | If of {cond: t; ifbody: t; elsebody: t; info: info}
      | IntLiteral of {literal: string; value: int64; info: info}
      | FloatLiteral of {literal: string; value: float; info: info}
      | CharLiteral of {literal: string; info: info}
      | StringLiteral of {literal: string; info: info}
      | Var of {name: string; info: info}
      | Let of {name: string; typ: t option; value: t; info: info}
      | Set of {name: t; value: t; info: info}
      | Block of {items: t list; info: info}
      | Tuple of {items: t list; info: info}
      | List of {items: t list; info: info}
      | Member of {base: t; member: string; info: info}
      | Return of {value: t; info: info}
      | Builtin of {builtin: Builtins.t; info: info}
      | Overload of {name: string; items: t list; info: info}
      | Empty of {info: info}
      | Type of {typ: t; info: info}
    [@@deriving compare, sexp, show {with_path= false}]

    and t1 = info t0

    and t = t1 ref

    let show_short node =
      let open Printf in
      let rec name_of node =
        match !node with
        | Extern {name; _} | Func {name; _} | Module {name; _} | Var {name; _} | Let {name; _} ->
            name
        | Import {path; _} -> String.concat ~sep:"." path
        | StringLiteral {literal; _} ->
            sprintf "\"%s\"" (String.sub literal ~pos:0 ~len:(Int.min (String.length literal) 10))
        | Builtin {builtin; _} -> Builtins.show builtin
        | Call {callee; _} -> name_of callee
        | Block _ -> ""
        | Param {name; _} -> name
        | Return {value; _} -> name_of value
        | _ -> "EXPR"
      and type_name_of node =
        match !node with
        | Extern _ -> "Extern"
        | Import _ -> "Import"
        | Module _ -> "Module"
        | Let _ -> "Let"
        | Block _ -> "Block"
        | Func _ -> "Func"
        | Var _ -> "Var"
        | Builtin _ -> "Builtin"
        | Call _ -> "Call"
        | Binop _ -> "Binop"
        | Param _ -> "Param"
        | _ -> "expr" in
      match !node with
      | Block _ -> "Block"
      | StringLiteral {literal; _} ->
          let literal = String.sub literal ~pos:0 ~len:(Int.min (String.length literal) 10) in
          let literal = String.escaped literal in
          sprintf "\"%s\"" literal
      | _ -> type_name_of node ^ "-" ^ name_of node
  end

  include Node0
  include Base.Comparable.Make (Node0)
  module Map = Map.With_first_class_module

  type node = t [@@deriving compare, sexp_of, show {with_path= false}]

  (** preorder fold *)
  let fold_info ~init ~f e =
    match !e with
    | Module {lines; _} -> List.fold ~init ~f lines
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
    match !e with
    | Module m -> Module {m with info= f e m.info}
    | Call m -> Call {m with info= f e m.info}
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
    | Module {info; _} -> info
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
  open Node

  let moduleNode name lines : node = ref @@ Module {name; lines; info= Info.create ()}
  let extern binding name typ : node = ref @@ Extern {binding; name; typ; info= Info.create ()}
  let import path names : node = ref @@ Import {path; names; info= Info.create ()}
  let var name : node = ref @@ Var {name; info= Info.create ()}
  let returnNode value : node = ref @@ Return {value; info= Info.create ()}
  let tuple items : node = ref @@ Tuple {items; info= Info.create ()}
  let list items : node = ref @@ List {items; info= Info.create ()}
  let block items : node = ref @@ Block {items; info= Info.create ()}
  let letNode name typ value : node = ref @@ Let {name; typ; value; info= Info.create ()}
  let setNode name value : node = ref @@ Set {name; value; info= Info.create ()}

  let ifNodeBase cond ifbody elsebody : node =
    ref @@ If {cond; ifbody; elsebody; info= Info.create ()}

  let rec ifNode cond ifbody eliflist elsebody =
    match eliflist with
    | (elif_cond, elif_body) :: rest ->
        ifNodeBase cond ifbody (ifNode elif_cond elif_body rest elsebody)
    | [] -> ifNodeBase cond ifbody elsebody

  let funcNode name ret_type params body =
    ref @@ Func {name; ret_type; params; body; info= Info.create ()}

  let callNode callee args : node = ref @@ Call {callee; args; info= Info.create ()}
  let binop (a, b, c) : node = ref @@ Call {callee= var a; args= [b; c]; info= Info.create ()}

  let int_literal s : node =
    ref @@ IntLiteral {literal= s; value= Int64.of_string s; info= Info.create ()}

  let float_literal s =
    ref @@ FloatLiteral {literal= s; value= Float.of_string s; info= Info.create ()}

  let char_literal s : node = ref @@ CharLiteral {literal= String.of_char s; info= Info.create ()}
  let string_literal s : node = ref @@ StringLiteral {literal= s; info= Info.create ()}
  let empty : node = ref @@ Empty {info= Info.create ()}
  let member base member : node = ref @@ Member {base; member; info= Info.create ()}
  let param idx name typ : node = ref @@ Param {idx; name; typ; info= Info.create ()}
end

include Node
