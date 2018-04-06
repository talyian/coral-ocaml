open Ast

type term = { name: string; node: Ast.node option }
type cons =
  | Term of term
  | Free of int
  | Type of string * cons list
  | Call of cons * cons list
  | Member of cons * string
  | Union of cons * cons

type graph = {
    mutable terms: term list;
    mutable edges: (term * cons) list;
}

type solution = {
  mutable active_terms: term list;
  mutable inactive_terms: term list;
}

module Graph = struct
  let create () = { terms=[]; edges=[] }
  let addTerm graph name astnode =
    graph.terms <- {name=name;node=Some(astnode)}::graph.terms;
    {name=name;node=Some(astnode)}

  let findTermByExpr graph astnode =
    try
      List.find (function | {node=Some n} when n = astnode -> true | _ -> false) graph.terms
    with exc ->
      Printf.eprintf "failed to find node %s\n" (nodeName astnode);
      Ast.show astnode;
      raise exc

  let addCons graph term cons = graph.edges <- (term, cons) :: graph.edges

  let rec cons_to_string = function
    | Term t -> t.name
    | Union (a, b) -> cons_to_string a ^ " <=> " ^ cons_to_string b
    | Free i -> "τ" ^ string_of_int i
    | Type (name, params) ->
       (match params with
        | [] -> name
        | p -> name ^ "[" ^ String.concat ", " (List.map cons_to_string params) ^ "]")
    | Call (callee, args) ->
       Printf.sprintf "call(%s, %s)" (cons_to_string callee) (
           List.map cons_to_string args |> String.concat ", ")
    | Member(base, mem) -> "member." ^ mem

  let show graph =
    let rec loop = function | [] -> ()
      | (t, c) :: xs ->
         Printf.printf "%20s :: %-20s\n" t.name (cons_to_string c);
         loop xs in loop graph.edges
end
                  
                  
let solve graph =
  Graph.show graph;
  0
