(* Ast Node - these represent an "expression"
 * They are created in the parser and can be tagged with additional information in each pass.
 *)

open Base

(* Represents a thing you import:
 * import foo.bar.baz
 * import foo.bar.baz (. as baz123)
 * import foo.bar.baz (...)
 * import foo.bar.baz (..., alpha, Bravo as beta)
 *)
type importType =
  | ModuleImport (* direct import *)
  | ModuleAlias of string (* import with alias *)
  | All (* star *)
  | ImpMember of string * string option
[@@deriving compare, sexp]

let pp_importType fmt = function ModuleImport -> () | _ -> ()

let show_importType = function
  | ModuleImport -> ""
  | ModuleAlias s -> " as " ^ s
  | All -> "*"
  | ImpMember (a, _) -> a

(* make sexp skinnier by hiding the refs *)
module Sexp_ref : sig
  type 'a t [@@deriving compare, sexp, show]

  val ( ! ) : 'a t -> 'a
  val ref : 'a -> 'a t
end = struct
  type 'a t = 'a ref [@@deriving compare]

  let pp pp_of_a formatter {contents} = pp_of_a formatter contents
  let t_of_sexp f s = ref @@ f s
  let sexp_of_t f {contents} = f contents

  let show (pp_contents : Formatter.t -> 'a -> unit) {contents} =
    let buffer = Buffer.create 8 in
    let formatter = Caml.Format.formatter_of_buffer buffer in
    pp_contents formatter contents ; Buffer.contents buffer

  let ( ! ) {contents} = contents
  let ref = ref
end

module Node = struct
  (* make sexp skinnier by just showing the name *)
  type var = {name: string} [@@deriving compare]

  let pp_var formatter {name} = Caml.Format.fprintf formatter "%s" name
  let var_of_sexp = function Sexp.Atom name -> {name} | List _ -> failwith "unexpected list"
  let sexp_of_var {name} = Sexp.Atom name

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
      | Yield of {value: t}
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

    and t = t0 Sexp_ref.t

    let rec show_short node =
      let open Printf in
      let rec name_of node =
        match Sexp_ref.( ! ) node with
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
        | TypeDecl {name; _} -> name
        | Binop {callee; _} -> name_of callee
        | Overload _ -> "Overload"
        | _ -> "EXPR"
      and type_name_of node =
        match Sexp_ref.( ! ) node with
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
        | TypeDecl _ -> "TypeDecl"
        | x -> ( match sexp_of_t0 x with List (Atom s :: _) -> s | _ -> "expr" ) in
      match Sexp_ref.( ! ) node with
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

  (* let fold_map ~(init : 'state) ~(f : 'state -> t -> 'state * t) (expr : t) =
   *   let bind1 f v =
   *     match Option.map ~f v with Some (init, v) -> (init, Some v) | None -> (init, None) in
   *   match !expr with
   *   | Module m ->
   *       let init, lines = List.fold_map ~init ~f m.lines in
   *       (init, ref @@ Module {m with lines})
   *   | Call c ->
   *       let init, callee = f init c.callee in
   *       let init, args = List.fold_map ~init ~f c.args in
   *       (init, ref @@ Call {callee; args})
   *   | Block {items} ->
   *       let init, items = List.fold_map ~init ~f items in
   *       (init, ref @@ Block {items})
   *   | Tuple {items} ->
   *       let init, items = List.fold_map ~init ~f items in
   *       (init, ref @@ Tuple {items})
   *   | List {items} ->
   *       let init, items = List.fold_map ~init ~f items in
   *       (init, ref @@ List {items})
   *   | Index c ->
   *       let init, callee = f init c.callee in
   *       let init, args = List.fold_map ~init ~f c.args in
   *       (init, ref @@ Call {callee; args})
   *   | Let x ->
   *       let init, value = f init x.value in
   *       let init, typ =
   *         match Option.map ~f:(f init) x.typ with
   *         | Some (init, typ) -> (init, Some typ)
   *         | None -> (init, None) in
   *       (init, ref @@ Let {name= x.name; typ; value})
   *   | StringLiteral _ | CharLiteral _ | IntLiteral _ | FloatLiteral _ | Var _ -> (init, expr)
   *   | Extern {name; binding; typ} ->
   *       let init, typ = f init typ in
   *       (init, ref @@ Extern {name; binding; typ})
   *   | Func {name; params; ret_type; body} ->
   *       let init, ret_type = bind1 (f init) ret_type in
   *       let init, params = List.fold_map ~init ~f params in
   *       let init, body = f init body in
   *       (init, ref @@ Func {name; params; ret_type; body})
   *   | Import _ | Param _ | Comment _ | Binop _ | If _ | Set _ | Member _ | Return _ | Builtin _
   *    |Overload _ | Empty | Type _ | Decorated _ | TypeDecl _ | TypeAlias _ ->
   *       failwith @@ Sexp.to_string [%sexp "unimplemented", (expr : node)] *)

  (* let fold ~init ~f expr = fst @@ fold_map ~init ~f:(fun state t -> (f state t, t)) expr *)
  let fold ~init ~f (expr : node) =
    match Sexp_ref.( ! ) expr with
    | Module x -> List.fold ~init ~f x.lines
    | Call x -> List.fold ~init:(f init x.callee) ~f x.args
    | Import _ -> init
    | Extern _ -> init
    | Func x ->
        let init = List.fold ~init ~f x.params in
        let init = Option.fold ~init ~f x.ret_type in
        let init = f init x.body in
        init
    | Param x -> Option.fold ~f ~init x.typ
    | Comment _ -> init
    | Binop x -> List.fold ~init ~f x.args
    | If x -> f (f (f init x.cond) x.ifbody) x.elsebody
    | IntLiteral _ -> init
    | FloatLiteral _ -> init
    | CharLiteral _ -> init
    | StringLiteral _ -> init
    | Var _ -> init
    | Let x ->
        let init = Option.fold ~init ~f x.typ in
        let init = f init x.value in
        init
    | Set x ->
        let init = f init x.name in
        f init x.value
    | Block x -> List.fold ~init ~f x.items
    | Tuple x -> List.fold ~init ~f x.items
    | List x -> List.fold ~init ~f x.items
    | Index x ->
        let init = f init x.callee in
        let init = List.fold ~init ~f x.args in
        init
    | Member x -> f init x.base
    | Return x -> f init x.value
    | Yield x -> f init x.value
    | Builtin _ -> init
    | Overload x -> List.fold ~f ~init x.items
    | Empty -> init
    | Type x -> f init x.typ
    | Decorated x -> f (f init x.attribute) x.target
    | TypeDecl x -> List.fold ~init ~f x.fields
    | TypeAlias x -> f init x.typ
end

(* A set of constructors for the Ast nodes. *)
module Make = struct
  open Node

  let ref = Sexp_ref.ref
  let moduleNode name lines : node = ref @@ Module {name; lines}
  let extern binding name typ : node = ref @@ Extern {binding; name; typ}
  let import path names : node = ref @@ Import {path; names}
  let var name : node = ref @@ Var {name}
  let returnNode value : node = ref @@ Return {value}
  let yieldNode value : node = ref @@ Yield {value}
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
