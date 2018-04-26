(* Type Inference Engine *)

let log_level = ref 1

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
  | Union of cons list

let rec cons_to_string = function
  | Term t -> as_rgb (5, 2, 3)  t.name
  | Union [a] -> cons_to_string a
  | Union (a :: xs) -> cons_to_string a ^ " | " ^ (cons_to_string (Union xs))
  | Union _ -> ""
  | Free i -> as_rgb (5, 5, 5) ("τ" ^ string_of_int i)
  | Type (name, params) ->
     (match params with
      | [] -> (as_rgb (4, 5, 3) name)
      | p ->
         let n = (as_rgb (4, 5, 3) name) in
         let params = (List.map cons_to_string p) in
         n ^ "[" ^ String.concat ", "  params ^ "]")
  | Call (callee, args) ->
     let args = List.map cons_to_string args |> String.concat ", " in
     Printf.sprintf "call(%s, %s)" (cons_to_string callee) args
  | Member(base, mem) -> "member." ^ mem

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int32)
module TermMap = Map.Make(struct
  type t = term
  let compare a b = compare a.name b.name
end)

(* A type environment *)
type typeEnv = {
  mutable names: term StringMap.t;
  mutable terms: term list;
  mutable edges: (term * cons) list;
}

(* An edge in a solution *)
type edge = {
    term: term;
    mutable cons: cons;
    mutable active: bool;
}

module EdgeSet = Set.Make(struct
  type t = edge
  let compare a b = compare (a.term, a.cons) (b.term, b.cons)
end)

module Edge = struct
  let equals a b = (a.term, a.cons) = (b.term, b.cons)

  let rec dependent_terms e =
    match e with
    | Term t -> [t]
    | Free f -> []
    | Type (name, c) -> (c |> List.map dependent_terms |> List.concat)
    | Call (callee, args) -> callee :: args |> List.map dependent_terms |> List.concat
    | Member (a, b) -> dependent_terms a
    | Union cases -> cases |> List.map dependent_terms |> List.concat

  let rec replace_term term cons = function
    | Term t -> if t.name = term.name then cons else Term t
    | Free f -> Free f
    | Type (name, c) -> Type(name, List.map (replace_term term cons) c)
    | Call (callee, args) ->
       Call(replace_term term cons callee,List.map (replace_term term cons) args)
    | Member (a, b) -> Member(replace_term term cons a, b)
    | Union cases -> Union(List.map (replace_term term cons) cases)
end

module Graph = struct

  let show (typeEnv:typeEnv) =
    let rec loop = function
      | [] -> ()
      | (t, c) :: xs ->
         Printf.printf "%20s :: %-20s\n"
           (Printf.sprintf "%20s" t.name)
           (cons_to_string c);
         loop xs in loop typeEnv.edges

  let create () = { names=StringMap.empty; terms=[]; edges=[] }
                    
  let addOptionalTerm (typeEnv:typeEnv) name astnode =
    let indexedname = function
      | 0 -> name
      | n -> name ^ "." ^ string_of_int n in
    let rec tryindex i =
      let iname = indexedname i in
      match StringMap.find_opt iname typeEnv.names with
      | Some(existing) -> tryindex (i + 1)
      | None -> iname in
    let name = tryindex 0 in
    let term = {name=name;node=astnode} in
    typeEnv.terms <- term::typeEnv.terms;
    typeEnv.names <- StringMap.add name term typeEnv.names;
    term
      
  let addTerm (typeEnv:typeEnv) name node = addOptionalTerm typeEnv name (Some node)

  let addCons (typeEnv:typeEnv) term cons = typeEnv.edges <- (term, cons) :: typeEnv.edges

  let findTermByExpr typeEnv astnode =
    match List.find_opt (function
              | {node=Some n} when n == astnode -> true
              | {node=Some(Def v)} ->
                 (match astnode with
                  | Def u -> u = v
                  | _ -> false)
              | _ -> false) typeEnv.terms with
    | None -> (show typeEnv; failwith "did not find expression")
    | Some(e) -> e

end

module Solver = struct
  type vertices = EdgeSet.t TermMap.t
  type solution = {
      mutable critical_terms: vertices;
      mutable dependent_terms: vertices;
    }
  let init (typeEnv:typeEnv) =
    let critical_terms =
      List.fold_left (fun map (term, cons) ->
          TermMap.update term
            (function
             |Some(edge_set) -> Some(EdgeSet.add {term=term;cons=cons;active=true} edge_set)
             | None -> Some(EdgeSet.singleton {term=term;cons=cons;active=true}))
            map
        ) TermMap.empty typeEnv.edges in
    { critical_terms=critical_terms;
      dependent_terms=TermMap.empty; }

  let show_labeled label solution =
    printf "- [%s] ----------------------------------------\n" label;
    let print_terms color_triple x =
      TermMap.bindings x
      |> List.map (fun (t, e) -> EdgeSet.elements e)
      |> List.concat
      |> List.iter
           (fun edge ->
             printf "%s :: %s\n"
               (as_rgb color_triple (sprintf "%20s" edge.term.name))
               (cons_to_string edge.cons))
    in
    print_terms (1, 5, 2) solution.critical_terms;
    print_terms (5, 3, 3) solution.dependent_terms

  let show = show_labeled "Solution"

  let delete_term solution term =
    {solution with critical_terms=TermMap.remove term solution.critical_terms}

  let defer_term solution term cons =
    (* printf "\tdeferring %s\n" term.name; *)
    {critical_terms=TermMap.remove term solution.critical_terms;
     dependent_terms=TermMap.update term (function
                         | Some(e) -> Some(EdgeSet.add {term=term;cons=cons;active=false} e)
                         | None -> Some(EdgeSet.singleton {term=term;cons=cons;active=false}))
                       solution.dependent_terms}
  let defer_term_all solution term =
    (* printf "\tdeferring_all %s\n" term.name; *)
    let edges = TermMap.find term solution.critical_terms in
    {critical_terms=TermMap.remove term solution.critical_terms;
     dependent_terms=TermMap.update term (function
                         | Some(e) -> Some(EdgeSet.union edges e)
                         | None -> Some(edges)) solution.dependent_terms}

  let substitute_term solution term cons =
    (* printf "\tsubstutituing %s\n" term.name; *)
    let sub_edge edge = {edge with cons=Edge.replace_term term cons edge.cons} in
    {critical_terms=TermMap.map (EdgeSet.map sub_edge) solution.critical_terms;
     dependent_terms=TermMap.map (EdgeSet.map sub_edge) solution.dependent_terms }

  let add_constraint_t cc term cons =
    TermMap.update term (function
        | Some(e) -> Some(EdgeSet.add {term=term;cons=cons;active=true} e)
        | None -> Some(EdgeSet.singleton {term=term;cons=cons;active=true})) cc

  let add_constraints solution constraints =
    let new_edges = List.fold_left (fun cc (t, c) -> add_constraint_t cc t c) solution.critical_terms constraints in
    {solution with critical_terms=new_edges}

  let rec add_term solution name index =
    let fullname = match index with | 0 -> name | n -> sprintf "%s.%d" name index in
    let term = {name=fullname;node=None} in
    match TermMap.find_opt term solution.critical_terms with
    | Some(t) -> add_term solution name (index + 1)
    | None ->
       match TermMap.find_opt term solution.dependent_terms with
       | Some(t) -> add_term solution name (index + 1)
       | None ->
          solution.critical_terms <- TermMap.add term EdgeSet.empty solution.critical_terms;
          term

  let rec instantiate instances solution = function
    | Free f ->
       (match IntMap.find_opt (Int32.of_int f) !instances with
        | Some(term) -> Term term
        | None ->
           let term = add_term solution "τ" 0 in
           instances := IntMap.add (Int32.of_int f) term !instances;
           Term term)
    | Type (name, params) -> Type(name, List.map (instantiate instances solution) params)
    | n -> n

  type unificationStatus =
    (* Reduced constraints *)
    | Success of (term * cons) list
    (* Could not reduce constraints *)
    | Defer of (term * cons) list
    (* constraints are invalid *)
    | Fail of string

  let combine_unification a b = match a, b with
    | Fail a, Fail b -> Fail (a ^ ", " ^ b)
    | Fail a, _ -> Fail a
    | _, Fail b -> Fail b
    | Success alist, Success blist -> Success (alist @ blist)
    | (Defer alist, Success blist) -> Success (alist @ blist)
    | (Success blist, Defer alist) -> Success (alist @ blist)
    | Defer alist, Defer blist -> Defer (alist @ blist)

  let rec unify solution a b =
    match a, b with
    | Type(x, xp), Type(y, yp) ->
       if x <> y then
         Fail ("Type Mismatch " ^ (cons_to_string a) ^ ", " ^ (cons_to_string b))
       else
         let results = List.map2 (unify solution) xp yp in
         List.fold_left combine_unification (Success []) results
    | Term x, Term y ->
       if x.name = y.name then Success [] else Success [x, Term y]
    | Free f, _ -> Fail (sprintf "free variable %d" f)
    | _, Free f -> Fail (sprintf "free variable %d" f)
    | (Type _ as y), Term x -> Success [x, y]
    | Term x, (Type _ as y) -> Success [x, y]
    | _, Term x -> unify solution b a
    | (Term _ as lhs, (Call(Type("Func", params), args)))
    | (Type _ as lhs, (Call(Type("Func", params), args))) ->
       let instances = ref IntMap.empty in
       let params = List.map (instantiate instances solution) params in
       let params_a = Array.of_list params in
       let args_a = Array.of_list args in
       if List.exists (function | Term {name="..."} -> true | _ -> false) params then
         let len = Array.length params_a in
         let res = Array.get params_a (len - 1) in
         unify solution lhs res
       else if Array.length args_a + 1 != Array.length params_a then
         Fail (sprintf
                 "parameter count mismatch (%s, %s)\n"
                 (cons_to_string a)
                 (cons_to_string b))
       else
         let len = Array.length args_a in
         let param_results = Array.map2 (unify solution) args_a (Array.sub params_a 0 len) in
         let result = unify solution lhs (Array.get params_a len) in
         Array.fold_left combine_unification result param_results
    | Term x, Call _ -> Defer [x, b]
    (* | Term x, Union cases -> *)
    | (Term _ as lhs, Union cases) | (Type _ as lhs, Union cases) ->
       let case_results = List.map (unify solution lhs) cases
       in (
         match List.find_opt (function | Fail s -> true | _ -> false) case_results with
         | Some f -> f
         | _ ->
            match List.find_opt (function | Success (a :: b:: c) -> true | _ ->false) case_results with
            | Some x -> Fail "multiple constraints out of union"
            | _ ->
               let all_cases = case_results
                               |> List.map (function | Success x -> x | Defer x -> x | _ -> [])
                               |> List.concat in
               Success all_cases)
    | _ -> Fail (sprintf
                   "unhandled unification:\n\t%s\n\t%s\n"
                   (cons_to_string a) (cons_to_string b))

  let unify_all solution constraints =
    (match constraints with
     | cons :: rest ->
        let (cresult, results) =
          List.fold_left
            (fun (cons_init, res) cc -> (cons_init, combine_unification res (unify solution cons cc)))
            (cons, Success [])
            rest in
        results
     | _ -> failwith "unification shape error")


  let rec simplify_cases = function
    | [n] -> [n]
    | Type (a, ax) as tt :: Union rest :: more -> simplify_cases (tt :: rest @ more)
    | Type (a, ax) as tt :: rest ->
         (match List.partition (function
             | Type (b, bx) when b = a && bx = ax -> true
             | Type (b, bx) when b = a -> printf "type  match???"; false
             | Type (b, bx) when bx = ax -> printf "prams match??"; false
             | Type _ -> printf "bad type?"; false
             | _ -> printf "oof"; false) rest with
          | [], not_equals -> (tt :: simplify_cases not_equals)
          | equals, not_equals -> simplify_cases (tt :: not_equals) )
      | cases -> cases
  and simplify_cons = function
    | Union cases ->
       (match simplify_cases cases with
        | [n] -> n
        | x -> Union x)
    | n -> n
  let step_union solution =
    let replace_edge e =
      let rec replace_cons = function
        | Union cases ->
           simplify_cons (Union cases)
        | Type(a, p) -> Type(a, List.map replace_cons p)
        | Call(x, ys) -> Call(replace_cons x, List.map replace_cons ys)
        | n -> n in
      (* printf "step_union :: \t%s:: %s\n \t:: %s\n"
       *   e.term.name
       *   (cons_to_string e.cons)
       *   (cons_to_string @@ replace_cons e.cons)
       * ; *)
      {e with cons=replace_cons e.cons}
    in
    let rep = TermMap.map (fun edges -> EdgeSet.map replace_edge edges) in
    {critical_terms= rep solution.critical_terms;
     dependent_terms= rep solution.dependent_terms}


  let step_term solution term constraints =
    (* show solution;
     * printf "Step: %s\n" term.name; *)
    (match constraints with
     | [] -> delete_term solution term
     | [Type _ as y] -> let s = defer_term solution term y in substitute_term s term y
     | [Term y] -> let s = defer_term solution term (Term y) in substitute_term s term (Term y)
     | [Union y] -> let s = defer_term solution term (Union y) in substitute_term s term (Union y)
     | [ Call(Type("Func", params), args) as y ] ->
        let sol = delete_term solution term in
        if term.name = "call.collatz_loop" then begin
            printf "Unifying callcollatz_loop\n";
            show_labeled "sol" solution
        end;
        (match unify sol (Term term) y with
         | Fail s -> printf "Error: %s\n" (as_color (Bold RED) s); sol
         | Success list ->
            printf "success\n";
            list |> List.iter (fun (tt, tc) ->
                        printf "  %s :: %s\n" tt.name (cons_to_string tc));
            add_constraints sol list
         | Defer list -> add_constraints sol list)
     | [ Call(Union cases, args) ] ->
        let sol = delete_term solution term in
        let status = List.map (fun c -> unify sol (Term term) (Call (c, args))) cases in
        (match List.partition (function | Success x -> true | _ -> false) status with
         | [Success x], _ -> add_constraints sol x
         | _ -> solution)
     | [ _ ] -> solution
     | x :: xs ->
        match unify_all solution (x :: xs) with
        | Success list ->
           let solution = defer_term_all solution term in add_constraints solution list
        | Defer list ->
           let solution = defer_term_all solution term in add_constraints solution list
        | Fail s -> printf "Error: %s\n" (as_color (Bold RED) s); solution
    )

  let step sol0 =
    (* show_labeled "Step" sol; *)
    let sol = step_union sol0 in
    let folder term edge_set solution =
      let edge_set = TermMap.find term solution.critical_terms in
      step_term solution term (edge_set |> EdgeSet.elements |> List.map (fun e -> e.cons))
    in TermMap.fold folder sol.critical_terms sol

  let fix solution =
    let rec pure_type = function
      | Type(n, p) -> List.for_all pure_type p
      | _ -> false in
    let types =
      solution.dependent_terms
      |> TermMap.bindings
      |> List.map (fun (tc, edges) -> EdgeSet.elements edges)
      |> List.concat
      |> List.filter (function | {cons=t} when pure_type t -> true | _ -> false)
    in
    List.fold_left (fun sol {term=t;cons=c} -> substitute_term sol t c) solution types

  let rec solve solution =
    let _show_labeled x y = if !log_level > 0 then show_labeled x y in
    solution
    |> tee (_show_labeled "Start")
    |> step |> step |> step |> step  |> step |> step |> fix |> step |> step |> fix
    |> tee (_show_labeled "Solved")
    |> tee (fun x -> flush stdout; x)
end

module Solver2 = struct
(* solve_with : 
 * takes an existing set of type terms and a new system of
 * constraints, reduces the constraints, 
 * and returns a new environment  *)                  
let solve_with
      environ (* already known terms *)
      system (* a set of constraints to solve *)  =
  ()

let withTerm name node (graph:typeEnv) =
  let term = Graph.addTerm graph name node in
  Graph.show graph;
  term, graph
let create_new_from_terms termPairs =
  let rec create_from_terms (graph:typeEnv) terms = function
    | [] -> terms, graph
    | (a, b) :: rest ->
       let tt, newgraph = withTerm a b graph in
       create_from_terms graph (tt :: terms) rest in
  let gg = Graph.create () in
  create_from_terms gg [] termPairs
                    
let test () =
  let [x; r; i], graph = create_new_from_terms [
     "x", Empty;
     "r", Empty;
     "i", Empty; ] in
  Graph.addCons graph i (Term x);
  Graph.addCons graph i (Type ("Int32", []));
  Graph.show graph;
  ()
    
end

let () = Solver2.test(); exit 0
    
(* let () =
 *   let type1 =
 *     printf "[Type Graph Test]  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
 *     let typeEnv = Graph.create () in
 *     let t1 = Graph.addTerm typeEnv "x" Empty in
 *     let r0 = Graph.addTerm typeEnv "r" Empty in
 *     let i0 = Graph.addTerm typeEnv "i" Empty in
 *     Graph.addCons typeEnv i0 (Type ("Int32", [])) ;
 *     Graph.addCons typeEnv i0 (Term t1);
 *     Graph.addCons typeEnv r0 (Call (Type ("Func", [Free 100; Free 100]), [Term t1]));
 *     Graph.show typeEnv;
 *     typeEnv
 *     |> Solver.init
 *     |> Solver.solve
 *     |> Solver.show
 *   in
 *   let type2 =
 *     printf "[Type Graph Test 2] ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
 *   in exit 0 *)
