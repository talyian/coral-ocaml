(* Type Inference Engine *)

open Ast
open Ansicolor
open Printf

module StringMap = Map.Make(String)

type term = { name: string; node: Ast.node option }
type cons =
  | Term of term
  | Free of int
  | Type of string * cons list
  | Call of cons * cons list
  | Member of cons * string
  | Union of cons * cons

type graph = {
  mutable names: term StringMap.t;
  mutable terms: term list;
  mutable edges: (term * cons) list;
}

type solution = {
  mutable names: term StringMap.t;
  mutable active_terms: term list;
  mutable inactive_terms: term list;
  mutable active_edges: (term * cons) ref list;
}

module Graph = struct
  let create () = { names=StringMap.empty; terms=[]; edges=[] }
  let addTerm (graph:graph) name astnode =
    let indexedname = function
      | 0 -> name
      | n -> name ^ "." ^ string_of_int n in
    let rec tryindex i =
      let iname = indexedname i in
      match StringMap.find_opt iname graph.names with
      | Some(existing) -> tryindex (i + 1)
      | None -> iname in
    let name = tryindex 0 in
    let term = {name=name;node=Some(astnode)} in
    graph.terms <- term::graph.terms;
    graph.names <- StringMap.add name term graph.names;
    term

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

module Solution = struct
(* Solution Functions *)
let createSolutionFromGraph graph = {
  names=StringMap.empty;
  active_terms=graph.terms; active_edges=List.map ref graph.edges;
  inactive_terms=[];
}

let term_equals a b = match a, b with
  | {node=Some(t)}, {node = Some(u)} when t == u -> true
  | {name=a;node=None}, {name=b;node=None} when a == b -> true
  | _ -> false
let term_c_equals a b =
  match a, b with
  | (Term {node=Some(t)}, Term {node=Some(u)}) when t == u -> true
  | _ -> false

let show (solution:solution) =
  let rec loop = function
    | {contents=(x, c)} :: xs ->
       let color = match List.find_opt (term_equals x) solution.active_terms with
         | None -> Color GREEN
         | Some(t) -> Color WHITE in
       printf "%20s :: %s\n"
         (as_color color (sprintf "%20s" x.name))
         (Graph.cons_to_string c);
       loop xs
    | [] -> () in
  printf "%s" (as_rgb (5, 5, 5) " [Solution]\n");
  loop (solution.active_edges |> List.sort compare)

let active_constraints_for_term solution term =
  let rec loop res = function
    | [] -> res
    | {contents=(t, c)} :: xs ->
       let res1 = if t.name = term.name then c :: res else res in
       loop res1 xs
  in loop [] solution.active_edges

let replace_term solution term constr =
  (* TODO: use get_referring_constraints *)
  let rec replace_c t = function
    | Term c_term as x ->
       if term_equals c_term term then constr
       else x
    | Type (n, params) -> Type (n, List.map (replace_c t) params)
    | Call (a, b) -> Call(replace_c t a, List.map (replace_c t) b)
    | x -> x in
  let rec loop = function
    | [] -> ()
    | {contents=(t, c)} as box :: xs ->
       box := (t, replace_c t c);
       loop xs in
  loop solution.active_edges

let addTerm solution name t =
  let rec nameloop name i =
    let indexedname = match i with
      | 0 -> name
      | n -> name ^ string_of_int n in
    match StringMap.find_opt indexedname solution.names with
    | Some(existing) -> nameloop name (i + 1)
    | None -> indexedname in
  let name = nameloop name 0 in
  let term = {name=name;node=None} in
  solution.names <- StringMap.add name term solution.names;
  solution.active_terms <- term :: solution.active_terms;
  term

let remove_constraint solution term cons =
  (* printf "removing constraint %s %s\n" (term.name) (Graph.cons_to_string cons); *)
  let c, d = List.partition (fun {contents=(t, c)} -> (term_equals t term) && cons = c) solution.active_edges in
  solution.active_edges <- d

let remove_term solution term =
  (* printf "Removing Term %s\n" term.name; *)
  let a, b = List.partition (fun t -> not (term_equals t term)) solution.active_terms in
  solution.active_terms <- a;
  solution.inactive_terms <- solution.inactive_terms @ b;

module IntMap = Map.Make(Int32)

let rec instantiate solution instances = function
  | Free i ->
     let i = Int32.of_int i in
     (match IntMap.find_opt i (!instances) with
      | Some(found) -> found
      | None ->
         let term = Term (addTerm solution "τ" Empty) in
         instances := IntMap.add i term (!instances);
         term)
  | n -> n

type unifyResult =
  | Success
  | Fail
  | Defer

let rec unify solution term left right =
  (* (printf "\t\tUnifying %s: %s <-> %s\n"
   *    term.name
   *    (Graph.cons_to_string left)
   *    (Graph.cons_to_string right)
   * ); *)
  match left, right with
  | (_, Free i) -> failwith "Unifying a free variable"
  | (Free i, _) -> failwith "Unifying a free variable"
  | (Term t, Term u) ->
     solution.active_edges <- ref (t, Term u) :: solution.active_edges;
     solution.active_terms <- u :: t :: solution.active_terms;
     (* printf "adding %s <-> %s\n" (t.name) (u.name); *)
     Success
  | (Type _, Term t) -> unify solution term right left
  | (Term t, (Type _ as y)) ->
     solution.active_edges <- ref (t, y) :: solution.active_edges;
     solution.active_terms <- t :: solution.active_terms;
     Success
  | (Call (a, a_args), Call (b, b_args)) ->
     printf "unifying 2 calls....\n";
     Defer
  | (_, Call _) -> unify solution term right left
  | (Call (Type ("Func", params), args), Term t) ->
     let instances = ref IntMap.empty in
     let params = List.map (instantiate solution instances) params in
     (match List.rev params with
     | [] -> failwith "bad params in unification"
     | retval :: rparams ->
        let rs = List.map2 (unify solution term) rparams (List.rev args) in
        let r2 = unify solution term retval (Term t) in
        Success)
  | (Call _ as c , other) ->
     solution.active_edges <- ref (term, right) :: ref (term, left) :: solution.active_edges;
     solution.active_terms <- term :: solution.active_terms;
     Defer
  | (Type (a, ap), Type (b, bp)) ->
     if a <> b then
       failwith (sprintf "bad unification: %s <-> %s\n" a b)
     else
       let result = List.map2 (unify solution term) ap bp in
       List.fold_left (fun a b -> match a, b with
                                  | Success, Success -> Success
                                  | Fail, _ -> Fail
                                  | _, Fail -> Fail
                                  | _ -> Defer
         ) Success result
  | _ ->
     show solution;
     printf "Unifying\n";
     printf "    %s\n" (Graph.cons_to_string left);
     printf "    %s\n" (Graph.cons_to_string right);
     failwith "Unhandled Unification\n"

(* at each step in the solution, we record progress made. If no progress, the solution is complete *)
type solveStepResult =
  | Progress
  | NoProgress

let rec solve graph =
  let solution = createSolutionFromGraph graph in
  Graph.show graph;
  show solution;
  let rec run_step i =
    List.map (runStep solution) solution.active_terms
    |> List.fold_left
         (fun a b -> match a, b with | NoProgress , NoProgress -> NoProgress | _ -> Progress) NoProgress
    |> (function
        | Progress -> run_step (i + 1)
        | NoProgress -> printf "Solution finished in %d steps\n" i) in
  run_step 0;
  solution;

and runStep solution term =
  (* printf "Inspecting term: %s\n" (as_rgb (5, 3, 2) term.name); *)
  match active_constraints_for_term solution term with
  | [] ->
     (* printf "\t\tNo constraints\n" *)
     remove_term solution term; Progress
  | [Term t] ->
     (* a single term constraint t -> u can be substituted into all references *)
     remove_term solution term;
     replace_term solution term (Term t); Progress
  | [Type (t, params)] ->
     (* a single type constraint be subtituted into all references *)
     remove_term solution term;
     replace_term solution term (Type (t, params)); Progress
  | cons ->
     (* at each call site we instantiate a concrete term for each Free type in func.params *)
     (* let rec handle_constraint cc =
      *   (match cc with
      *    | Call (Type ("Func", params), args) ->
      *       let instances = ref IntMap.empty in
      *       let params = List.map (instantiate solution instances) params in (
      *           let a = List.rev args in
      *           match List.rev params with
      *           | [] -> failwith "something bad happened -- function doesn't have return type"
      *           | c :: b ->
      *           unify solution term c (Term term);
      *           let result = List.map2 (unify solution term) a b in
      *           remove_constraint solution term cc
      *         )
      *    | _ -> ())
      * in
      * List.iter handle_constraint cons; *)

     (* If we have N type / term constraints for a term, we can reduce the set
      * and replace with N-1 terms.
      * TODO -- we can remove the term from the active list completely if we also substitute
      * in the final unification result into all the replacement terms *)

     List.iter (remove_constraint solution term) cons;
     flush stdout;
     (let loop c1 c2 =
        (* printf "%s <-> %s\n" (Graph.cons_to_string c1) (Graph.cons_to_string c2); *)
        (match unify solution term c1 c2 with
         | Defer -> c2
         | Success -> c2
         | Fail -> c2)
      in
      match List.fold_left loop (Term term) cons with
      | _ -> NoProgress);
end
