open Base
open Coral_core

module RecordType = struct
  type 'a t = {name: string; fields: (int * string option * 'a) list} [@@deriving compare]

  let index_of t target_name =
    List.find_map t.fields ~f:(function
      | i, Some name, v when String.equal name target_name -> Some i
      | _ -> None)

  let type_of t target_name =
    List.find_map t.fields ~f:(function
      | i, Some name, v when String.equal name target_name -> Some v
      | _ -> None)
end

(* TypeSpec is the type solver's idea of a "type" It's more like a "compile-time-known
     constraint" *)
type t =
  | Any
  | ConstInt of int64
  | ConstFloat of float
  | ConstString of string
  | Const of Builtins.t
  | Record of (string option * t) list
  | RecordType of t RecordType.t
  | InstanceOf of t (* e.g. 3 :: InstanceOf Int *)
  | TypeFor of t (* inverse of instanceof -- Int :: TypeFor (Const 3) *)
  | Applied of t * t list
  | And of t * t
  (* And means that multiple typespecs apply to a term.*)
  | Overload of t list
  (* Overload is the OR for typespec. it means a term can be at least one of the
       included typespecs.

       It can be created from an set of overloaded functions
       func foo(a): ...
       @overload
       func foo(b): ...

       This means at any point foo can be one of the following types:
       Func[][A]
       Func[][B]

       Overloaded values can arise from conditionals
       let foo = if rand () then 3 else "3"

       Or also from declaring an untagged union:
       let foo : union { Int; String } = 3

       let foo = (3 | "3")
  *)
  | Error
[@@deriving compare]

let rec sexp_of_t = function
  | Const b -> Builtins.sexp_of_t b
  | ConstInt i -> Int64.sexp_of_t i
  | Record fields ->
      let sexp_of_field (name, value) =
        match name with
        | None -> [sexp_of_t value]
        | Some n -> [String.sexp_of_t n; sexp_of_t value] in
      Sexp.List (List.concat_map ~f:sexp_of_field fields)
  | RecordType rc -> List [Atom "RecordType"; Atom rc.name]
  | Any -> Sexp.Atom "*"
  | Error -> Sexp.Atom "Error"
  | ConstFloat f -> Float.sexp_of_t f
  | ConstString s -> Atom s
  | InstanceOf x -> List [Atom "instance_of"; sexp_of_t x]
  | TypeFor x -> List [Atom "type_for"; sexp_of_t x]
  | Applied (a, bs) -> List ([Sexp.Atom "Applied"; sexp_of_t a] @ List.map ~f:sexp_of_t bs)
  | And (a, b) -> List [Atom "a"; sexp_of_t a; sexp_of_t b]
  | Overload xs -> List ([Sexp.Atom "Overload"] @ List.map ~f:sexp_of_t xs)

let rec show x = show0 0 x

and show0 n = function
  | Any -> "*"
  | InstanceOf t -> ":" ^ show0 4 t
  | TypeFor t -> "@@" ^ show t
  | ConstInt x -> Int64.to_string x ^ "i"
  | ConstFloat x -> Float.to_string x ^ "f"
  | ConstString s -> "'" ^ String.escaped s ^ "'"
  | Const b -> Builtins.show b
  | Record record as r -> "{" ^ Sexp.to_string [%sexp (r : t)] ^ "}"
  | RecordType _ as r -> Sexp.to_string [%sexp (r : t)]
  | Applied (a, b) -> show0 2 a ^ "[" ^ (String.concat ~sep:", " @@ List.map ~f:show b) ^ "]"
  | And (a, b) -> show0 2 a ^ " and " ^ show0 2 b
  | Overload items -> "overload:(" ^ (String.concat ~sep:"," @@ List.map ~f:show items) ^ ")"
  | Error -> "error"

let get_type = function
  | TypeFor t -> Const (Builtins.Custom "Type")
  | ConstInt _ -> Const Builtins.INT64
  | ConstString _ -> Const Builtins.STR
  | ConstFloat _ -> Const Builtins.FLOAT64
  | InstanceOf x -> x
  | t -> failwith @@ Sexp.to_string [%sexp "unknown type", (t : t)]

let instance_of = function TypeFor x -> x | x -> InstanceOf x
