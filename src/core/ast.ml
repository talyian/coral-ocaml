(* Ast Node - these represent an "expression"
 * They are created in the parser and can be tagged with additional information in each pass.
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

module Node = struct
  (* make sexp skinnier by just showing the name *)
  type var = {name: string} [@@deriving compare]

  let pp_var formatter {name} = Caml.Format.fprintf formatter "%s" name
  let var_of_sexp = function Sexp.Atom name -> {name} | List _ -> failwith "unexpected list"
  let sexp_of_var {name} = Sexp.Atom name

  (* make sexp skinnier by hiding the refs *)
  type 'a sexp_ref = 'a ref [@@deriving compare]

  let pp_sexp_ref pp_of_a formatter {contents} = pp_of_a formatter contents
  let sexp_ref_of_sexp f s = ref @@ f s
  let sexp_of_sexp_ref f {contents} = f contents

  module Node0 = struct
    type t0 =
      | Module of {name: string; lines: t list}
      | Call of {callee: t; args: t list}
      | Import of {path: string list; names: importType list}
      | Extern of {binding: string; name: string; typ: t}
      | Func of {name: string; ret_type: t option; params: t list; body: t}
      | Param of {idx: int; name: string; typ: t option}
      | Comment of {comment: string}
      | Binop of {callee: t; args: t list}
      | If of {cond: t; ifbody: t; elsebody: t}
      | IntLiteral of {literal: string; value: int64}
      | FloatLiteral of {literal: string; value: float}
      | CharLiteral of {literal: string}
      | StringLiteral of {literal: string}
      | Var of var
      | Let of {name: string; typ: t option; value: t}
      | Set of {name: t; value: t}
      | Block of {items: t list}
      | Tuple of {items: t list}
      | List of {items: t list}
      | Index of {callee: t; args: t list}
      | Member of {base: t; member: string}
      | Return of {value: t}
      | Builtin of {builtin: Builtins.t}
      | Overload of {name: string; items: t list}
      | Empty
      | Type of {typ: t}
      | Decorated of {attribute: t; target: t}
      | TypeDecl of
          { name: string
          ; metatype: string (* TODO: higher-ordered/structured metatype? *)
          ; fields: t list }
      | TypeAlias of {name: string; typ: t}
    [@@deriving compare, sexp, show {with_path= false}]

    and t = t0 sexp_ref

    let rec show_short node =
      let open Printf in
      let rec name_of node =
        match !node with
        | Extern {name; _} | Func {name; _} | Module {name; _} | Var {name; _} | Let {name; _} ->
            name
        | Import {path; _} -> String.concat ~sep:"." path
        | StringLiteral {literal; _} ->
            sprintf "\"%s\"" (String.sub literal ~pos:0 ~len:(Int.min (String.length literal) 10))
        | IntLiteral {literal; _} -> literal
        | FloatLiteral {literal; _} -> literal
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
      | Module {name; _} -> ( match name with "" -> "<module>" | n -> n )
      | Func {name; _} -> name
      | Block _ -> "Block"
      | Tuple {items; _} -> "(" ^ (String.concat ~sep:", " @@ List.map ~f:show_short items) ^ ")"
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

  let fold_map ~(init : 'state) ~(f : 'state -> t -> 'state * t) (expr : t) =
    match !expr with
    | Module m ->
        let init, lines = List.fold_map ~init ~f m.lines in
        (init, ref @@ Module {m with lines})
    | Call c ->
        let init, callee = f init c.callee in
        let init, args = List.fold_map ~init ~f c.args in
        (init, ref @@ Call {callee; args})
    | Block {items} ->
        let init, items = List.fold_map ~init ~f items in
        (init, ref @@ Block {items})
    | Tuple {items} ->
        let init, items = List.fold_map ~init ~f items in
        (init, ref @@ Tuple {items})
    | List {items} ->
        let init, items = List.fold_map ~init ~f items in
        (init, ref @@ List {items})
    | Import _ | Extern _ | Func _ | Param _ | Comment _ | Binop _ | If _ | IntLiteral _
     |FloatLiteral _ | CharLiteral _ | StringLiteral _ | Var _ | Let _ | Set _ | Index _
     |Member _ | Return _ | Builtin _ | Overload _ | Empty | Type _ | Decorated _ | TypeDecl _
     |TypeAlias _ ->
        (init, expr)

  let fold ~init ~f expr = fst @@ fold_map ~init ~f:(fun state t -> (f state t, t)) expr
end

(* A set of constructors for the Ast nodes. *)
module Make = struct
  open Node

  let moduleNode name lines : node = ref @@ Module {name; lines}
  let extern binding name typ : node = ref @@ Extern {binding; name; typ}
  let import path names : node = ref @@ Import {path; names}
  let var name : node = ref @@ Var {name}
  let returnNode value : node = ref @@ Return {value}
  let tuple items : node = ref @@ Tuple {items}
  let list items : node = ref @@ List {items}
  let block items : node = ref @@ Block {items}
  let letNode name typ value : node = ref @@ Let {name; typ; value}
  let setNode name value : node = ref @@ Set {name; value}
  let ifNodeBase cond ifbody elsebody : node = ref @@ If {cond; ifbody; elsebody}

  let rec ifNode cond ifbody eliflist elsebody =
    match eliflist with
    | (elif_cond, elif_body) :: rest ->
        ifNodeBase cond ifbody (ifNode elif_cond elif_body rest elsebody)
    | [] -> ifNodeBase cond ifbody elsebody

  let funcNode name ret_type params body = ref @@ Func {name; ret_type; params; body}
  let callNode callee args : node = ref @@ Call {callee; args}
  let binop (a, b, c) : node = ref @@ Call {callee= var a; args= [b; c]}
  let int_literal s : node = ref @@ IntLiteral {literal= s; value= Int64.of_string s}
  let float_literal s = ref @@ FloatLiteral {literal= s; value= Float.of_string s}
  let char_literal s : node = ref @@ CharLiteral {literal= String.of_char s}
  let string_literal s : node = ref @@ StringLiteral {literal= s}
  let empty : node = ref @@ Empty
  let member base member : node = ref @@ Member {base; member}
  let param idx name typ : node = ref @@ Param {idx; name; typ}
  let typeDecl name metatype fields = ref @@ TypeDecl {name; metatype; fields}
  let typeAlias name typ = ref @@ TypeAlias {name; typ}
  let index callee args = ref @@ Index {callee; args}
  let attribute (attr : string) _params target = ref @@ Decorated {attribute= var attr; target}
end

include Node
