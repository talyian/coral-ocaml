open Base

module type Data = sig
  type t [@@deriving sexp_of]

  val string_of_t : t -> string
end

module Make (Data : Data) = struct
  type term = int [@@deriving compare]

  let sexp_of_term = sexp_of_int

  type expr =
    | ConstInt of int64
    | Term of term
    | Type_of of expr
    | Type_for of expr
    | Applied of expr * expr list
    | StructDef of {name: string; fields: (string * term) list}
  [@@deriving compare, sexp_of]

  module Expr = struct
    type t = expr [@@deriving compare]

    include Comparable.Make (struct
      type t = expr [@@deriving compare, sexp_of]
    end)
  end

  type fact = Is of term * expr [@@deriving sexp_of]

  type t =
    { debug: bool
    ; next_term: int
    ; terms: term list
    ; extra_data: Data.t list
    ; (* tracks all the instances for parametric polymorphism. TODO: keeping
         these globally is a bit inefficient because in order to instantiate a
         free var we have to scan the entire environment. We could optimize this
         by storing a subgraph of nodes that depends on each free variable. *)
      free_vars: term list
    ; (* Keeps track of our current known constraints *)
      facts: fact list
    ; exprs: term Map.M(Expr).t }

  let data_for_term term t =
    List.map2_exn t.terms t.extra_data ~f:(fun t data -> if t = term then Some data else None)
    |> List.find_map_exn ~f:Fn.id

  let empty =
    { debug= false
    ; next_term= 0
    ; terms= []
    ; extra_data= []
    ; free_vars= []
    ; facts= []
    ; exprs= Map.empty (module Expr) }

  (* Adds a term, which is a vertex in the type graph *)
  let add_term data t =
    let new_term : term = t.next_term in
    let t =
      { t with
        terms= new_term :: t.terms
      ; extra_data= data :: t.extra_data
      ; next_term= t.next_term + 1 } in
    (t, new_term)

  (* Adds an edge to the type graph *)
  let term_is term expr t =
    Stdio.printf "%s      %s\n"
      (Data.string_of_t @@ data_for_term term t)
      (sexp_of_expr expr |> Sexp.to_string) ;
    {t with facts= Is (term, expr) :: t.facts}

  (* Adds a memoized term for a constraint to the graph *)
  let const_term_for expr t =
    match Map.find t expr with Some term -> (t, term) | None -> failwith "oops"
end
