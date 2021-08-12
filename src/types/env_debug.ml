open Base

module type Data = sig
  type t [@@deriving sexp_of]

  val string_of_t : t -> string
end

module Term = struct
  module Term = struct
    type t = {ordinal: int}

    let compare {ordinal= a} {ordinal= b} = Int.compare a b
    let sexp_of_t {ordinal= a} = sexp_of_int a
  end

  include Term
  include Comparable.Make (Term)
end

module Make (Data : Data) = struct
  type term = Term.t [@@deriving compare, sexp_of]

  let term i = {Term.ordinal= i}

  type expr =
    | ConstInt of int64
    | Term of term
    | StructDef of {name: string; fields: (string * term) list}
  [@@deriving compare, sexp_of]

  type t =
    { debug: bool
    ; (* terms are just an id, so give them  meaningful names in the env *)
      term_names: string Map.M(Term).t
    ; (* maps a term back to the ast expr *)
      term_data: Data.t Map.M(Term).t }

  let empty : t =
    {debug= false; term_names= Map.empty (module Term); term_data= Map.empty (module Term)}

  let new_term t = term (1 + Map.length t.term_names)

  let add_term data t =
    let name = Data.string_of_t data in
    Stdio.printf "Add: %s\n" name ;
    let new_term = new_term t in
    let t =
      { t with
        term_names= Map.set ~key:new_term ~data:name t.term_names
      ; term_data= Map.set ~key:new_term ~data t.term_data } in
    (t, new_term)

  let const_term_for expr t =
    let name = Sexp.to_string @@ sexp_of_expr expr in
    let new_term = new_term t in
    let t = {t with term_names= Map.set ~key:new_term ~data:name t.term_names} in
    (t, new_term)

  let show_term env term = Map.find_exn env.term_names term

  let show_expr env = function
    | ConstInt x -> Int64.to_string x
    | Term term -> show_term env term
    | StructDef {name; fields} ->
        Printf.sprintf "Struct-%s{%s}" name
        @@ String.concat ~sep:", "
        @@ List.map ~f:(fun (name, term) -> name ^ "=" ^ show_term env term) fields

  let term_is term (expr : expr) env =
    let term_name = Map.find_exn env.term_names term in
    Stdio.printf "Is: %s -> %s\n" term_name (show_expr env expr) ;
    env
end
