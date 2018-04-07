(* Type Inference Engine *)

open Ast
open Ansicolor
open Printf

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
  mutable active_edges: (term * cons) ref list;
  mutable inactive_edges: (term * cons) ref list;
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
    | Term t -> as_rgb (5, 2, 3)  t.name
    | Union (a, b) -> cons_to_string a ^ " <=> " ^ cons_to_string b
    | Free i -> as_rgb (5, 5, 5) ("τ" ^ string_of_int i)
    | Type (name, params) ->
       (match params with
        | [] -> (as_rgb (4, 5, 3) name)
        | p -> (as_rgb (4, 5, 3) name) ^ "[" ^ String.concat ", " (List.map cons_to_string params) ^ "]")
    | Call (callee, args) ->
       let args = List.map cons_to_string args |> String.concat ", " in
       Printf.sprintf "call(%s, %s)" (cons_to_string callee) args
    | Member(base, mem) -> "member." ^ mem

  let show graph =
    let rec loop = function
      | [] -> ()
      | (t, c) :: xs ->
         Printf.printf "%20s :: %-20s\n" ((Printf.sprintf "%20s" t.name)) (cons_to_string c);
         loop xs in loop graph.edges
end

(* Solution Functions *)
let term_equals a b = match a, b with | {node=Some(t)}, {node = Some(u)} when t == u -> true | _ -> false
let term_c_equals a b =
  match a, b with
  | (Term {node=Some(t)}, Term {node=Some(u)}) when t == u -> true
  | _ -> false
let active_constraints_for_term solution term =
  let rec loop res = function
    | [] -> res
    | {contents=(t, c)} :: xs ->
       (t.node, term.node)
       |> (function | (Some a), (Some b) when a == b -> c :: res | _ -> res)
       |> (flip loop) xs
  in loop [] solution.active_edges
           
let replace_term solution term constr =
  (* TODO: use get_referring_constraints *)
  let rec replace_c = function
    | Term c_term as x -> if term_equals c_term term then constr else x
    | Type (n, params) -> Type (n, List.map replace_c params)
    | Call (a, b) -> Call(replace_c a, List.map replace_c b)
    | x -> x in                             
  let rec loop = function
    | [] -> ()
    | {contents=(t, c)} as box :: xs ->
       box := (t, replace_c c);
       loop xs in
  loop solution.active_edges;
  loop solution.inactive_edges

let addTerm solution name t =
  let term = {name=name;node=None} in
  solution.active_terms <- term :: solution.active_terms;
  term
       
let remove_constraint solution term cons =
  printf "removing constraint %s %s\n" (term.name) (Graph.cons_to_string cons);
  let c, d = List.partition (fun {contents=(t, c)} -> (term_equals t term) && cons = c) solution.active_edges in
  solution.active_edges <- d
  (* solution.inactive_edges <- solution.inactive_edges @ c *)
  
let remove_term solution term =
  let a, b = List.partition (fun t -> not (term_equals t term)) solution.active_terms in
  let c, d = List.partition (fun {contents=(t, c)} -> not (term_equals t term)) solution.active_edges in
  solution.active_terms <- a;
  solution.inactive_terms <- solution.inactive_terms @ b;
  solution.active_edges <- c;
  solution.inactive_edges <- solution.inactive_edges @ d

module IntMap = Map.Make(Int32)
                 
let rec instantiate solution instances =
  let get_or_set i =
    try
      IntMap.find (Int32.of_int i) !instances
    with Not_found ->
      let term = addTerm solution ("α" ^ string_of_int i) Empty in
      (instances := IntMap.add (Int32.of_int i) term (!instances); term)
  in
  function | Free i -> Term (get_or_set i) | n -> n
                                                    
let rec unify solution left right =
  match left, right with
  | (Term t, Term u) -> solution.active_edges <- ref (t, Term u) :: solution.active_edges
  (* this is wrong! we should instantiate at application site *)
  | (Term t, Free i) -> solution.active_edges <- ref (t, Free i) :: solution.active_edges
  | _ -> ()
    
let show (solution:solution) =
  let rec loop color = function
    | {contents=(x, c)} :: xs ->
       printf "%20s :: %s\n"
         (as_color color (sprintf "%20s" x.name))
         (Graph.cons_to_string c);
       loop color xs
    | [] -> () in
  printf "%s" (as_rgb (5, 5, 5) " [Solution]\n");
  loop (Color WHITE) (solution.active_edges |> List.sort compare);
  loop (Underline WHITE) (solution.inactive_edges |> List.sort compare)

       
let rec solve graph =
  let solution = {
      active_terms=graph.terms; active_edges=List.map ref graph.edges;
      inactive_terms=[]; inactive_edges = []
    } in
  Graph.show graph;
  show solution;
  let rec loop s = function | [] -> () | t :: xs -> inspect s t; loop s xs in
  loop solution solution.active_terms;
  show solution;
  0

and inspect solution term =
  (* printf "Inspecting term: %s\n" (as_rgb (5, 3, 2) term.name); *)
  match active_constraints_for_term solution term with
  | [] -> ()
  (* printf "\t\tNo constraints\n" *)
  | [Term t] ->
     (* a single term constraint can be substituted into all references *)
     replace_term solution term (Term t);
     remove_term solution term                  
  | [Type (t, params)] ->
     (* a single type constraint be subtituted into all references *)
     replace_term solution term (Type (t, params));
     remove_term solution term
  | cons ->
     let rec handle_constraint cc =
       (match cc with
        | Call (Type ("Func", params), args) ->
           (* at each call site we instantiate a concrete term for each Free type in func.params *)
           let instances = ref IntMap.empty in
           let params = List.map (instantiate solution instances) params in (
               let a = List.rev args in
               let c :: b = List.rev params in
               unify solution c (Term term);
               List.iter2 (unify solution) a b;
               remove_constraint solution term cc
             )
        | _ -> ())
     in
     List.iter handle_constraint cons;
     (* printf "\t\t%d constraints\n" (List.length cons); *)
     (* (let rec loop = function
      *    | [] -> ()
      *    | c :: xs -> printf "\t\t%s\n" (Graph.cons_to_string c); loop xs
      *  in loop cons) *)
     ()
