(* Version 2.0 of Type Solver System
  (╯°□°）╯︵ ┻━┻

  In this version it is assumed we're doing incremental typing.
  This means that `solve` takes as input:
    env: a type environment 
    graph: a new type constraint graph
  and returns as output:
    env': a new type environment

Incremental typing makes codegen more difficult. For example, the resolved type of a function must be as generic as possible. typeof (func s: s + s) is pretty much whatever "+" can be applied to, which is many things, and not even necessarily the same types: (T, T -> T). For example, (Tuple[T] + Tuple[U] = Tuple[T, U]).

At each call site then during codegen, we need to generate an instantiation.

   func foo(s): return s + s
   // generates foo$Int32
   foo 1
   // generates foo$Float64
   foo 1.5
   // reuses foo$Int32
   foo 2
*)

(* module Environment = functor (Node: sig type t val show: t -> string end) -> struct
 *   type node = Node.t
 *   module StringMap = Map.Make(String)
 * 
 *   type coralType =
 *     | Named of string (\* A named type, such as "Int32" *\)
 *     | Free of int (\* A free type variable *\)
 *     | Parameterized of string * coralType list
 *   let rec type_to_string = function
 *     | Named s -> s
 *     | Free n -> "τ" ^ string_of_int n
 *     | Parameterized (n, p) ->
 *         (match p with
 *          | [] -> n
 *          | _ -> let p = List.map type_to_string p in
 *                 n ^ "[" ^ (String.concat ", " p) ^ "]")
 *                                           
 *   type term = { name: string; node: node option; coraltype: coralType }
 *   type environment = {
 *       parents: environment list;
 *       _names: node StringMap.t;
 *       _terms: term list
 *   }
 *                        
 *   let empty = {parents=[]; _terms=[]; _names=StringMap.empty}
 * 
 *   let addTerm env name node tt = 
 *     let rec addTerm env realname node i =
 *       let name = match i with | 0 -> realname | n -> realname ^ "." ^ string_of_int n in
 *       match StringMap.mem name env._names with
 *       | false -> name, {
 *          env with
 *          _names = StringMap.add name node env._names;
 *          _terms = {name=name;node=Some(node);coraltype=tt} :: env._terms }
 *       | true -> addTerm env realname node (i + 1)
 *     in addTerm env name node 0
 * 
 *   let getTerms env = env._terms
 *                        
 *   let showTerms env =
 *     List.iter
 *       (fun t -> Printf.printf
 *         "%20s = %s\n" ("[" ^ t.name ^ "]")
 *         (type_to_string t.coraltype))
 *       env._terms
 * end *)

module GraphF =
  functor (Node: sig
               type t
               val show: t -> string
               val empty: t
             end) -> struct

  type configTy = { mutable debug: bool }
  let config = { debug = true }
  type cons =
    | Free of int
    | Term of string
    | Type of string * cons list
    | Call of cons * cons list
    | OneOf of cons list
    | AllOf of cons list
    | Member of cons * string
  let rec cons_to_string = function
    | Free n -> "free." ^ string_of_int n
    | Term s -> "@" ^ s
    | Type (n, []) -> n
    | Type (n, p) ->
       n ^ "[" ^ (String.concat ", " (List.map cons_to_string p)) ^ "]"
    | Call (a, p) ->
       cons_to_string a ^ "(" ^ (String.concat ", " (List.map cons_to_string p)) ^ ")"
    | OneOf p -> "(" ^ (String.concat "|" (List.map cons_to_string p)) ^ ")"
    | AllOf p -> "(" ^ (String.concat " & " (List.map cons_to_string p)) ^ ")"
    | Member(a, m) -> cons_to_string a ^ "." ^ m
                         
  type term = { name: string }
  module TermMap = Map.Make(struct type t = term let compare a b = compare a.name b.name end)
  module StringMap = Map.Make(String)
  module IntMap = Map.Make(struct type t = int let compare = compare end)
                          
  type graph = {
      parent: graph option;
      terms: term StringMap.t;
      constraints: cons TermMap.t;
  }
  let empty = {terms=StringMap.empty; constraints=TermMap.empty; parent=None}
  let childOf parent = {empty with parent=Some(parent)}

  let rec findTerm graph name =
    match StringMap.find_opt name graph.terms with
    | Some(n) -> Some(n)
    | None ->
       match graph.parent with
       | Some(p) -> findTerm p name
       | None -> None
  let addTerm graph name node =
    let rec add_loop i =
      let newname = match i with
        | 0 -> name
        | n -> name ^ "." ^ string_of_int n in
      match findTerm graph newname with
      | Some(_) -> add_loop (i + 1)
      | None ->
         let term = {name=newname} in
         term, {graph with terms=StringMap.add newname term graph.terms} in
    add_loop 0

  let constrain graph term cons =
    let update = function
      | None -> Some(cons)
      | Some(AllOf list) -> Some(AllOf (cons :: list))
      | Some(x) -> Some(AllOf [cons; x]) in
    { graph with constraints=TermMap.update term update graph.constraints}

  let showColor rgb graph =
    graph.constraints
    |> TermMap.iter (fun term cons ->
           Printf.printf "%s :: %s\n"
             (Ansicolor.as_rgb rgb @@ Printf.sprintf "%20s" term.name)
             (cons_to_string cons))
  let show = showColor (3,4,5)
                       
  let replace_term term cons graph =
    (* Printf.printf "Replacing %s with %s\n" term.name (cons_to_string cons); *)
    let rec replace_func
              (tt:term)
              (tc:cons)
      =
      match tc with 
      | Term s when s = term.name -> cons
      | Type (n, p) -> Type (n, List.map (replace_func tt) p)
      | Call (callee, args) -> Call((replace_func tt) callee, List.map (replace_func tt) args)
      | OneOf p -> OneOf(p |> List.filter ((<>) (Term tt.name)) |> List.map (replace_func tt))
      | AllOf p -> AllOf(p |> List.filter ((<>) (Term tt.name)) |> List.map (replace_func tt))
      | _ as f -> f in
    {graph with
      constraints =
        graph.constraints
        |> TermMap.mapi replace_func
        |> TermMap.remove term }

  (* add : cons -> cons option -> cons
     Adds two constraints, merging AllOfs to avoid nesting *)
  let addcons cons foo = match cons, foo with
    | cc, None -> cc
    | AllOf y, Some(AllOf x) -> AllOf (y @ x)
    | cons, Some(AllOf x) -> AllOf (cons :: x)
    | AllOf y, Some(cc) -> AllOf (cc :: y)
    | a, Some(b) -> AllOf [a; b]
      
  let shelve term cons graph =
    {graph with constraints =
                  TermMap.update term
                    (fun x -> Some(addcons cons x)) graph.constraints}

  type unification =
    | Fail of string
    | Success of graph * (term * cons) list
                               
  let unify_collect graph =
    List.fold_left (fun a b ->
      match a, b with
      | Fail s, Fail t -> Fail (s ^ "; " ^ t)
      | (Fail s, _) | (_, Fail s) -> Fail s
      | Success (gg, s), Success (hh, t) -> Success (graph, s @ t)) (Success (graph, []))
  let rec unify graph cons1 cons2 =
    match cons1, cons2 with
    | Type (a, ap), Type(b, bp) ->
       if a = b then
         unify_collect graph @@ List.map2 (unify graph) ap bp 
       else
         Fail (Printf.sprintf "mismatch: %s, %s" a b)
    | Term x, Term y -> Success (graph, [{name=x}, Term y])
    | (Term x, (Type _ as y)) | (Type _ as y, Term x) -> Success (graph, [{name=x}, y])
    | Term term, Call (Type("Func", params), args) ->
       let arg1 = args @ [Term term] in
       let instantiate (gg, map, pp) = (function
           | Free n -> (match IntMap.find_opt n map with
               | None ->
                  let nt, gg = addTerm gg "free" Node.empty in
                  gg, IntMap.add n nt map, (Term nt.name) :: pp
               | Some(t) -> gg, map, Term t.name :: pp)
           | n -> gg, map, n :: pp) in
       let graph, _, params =
         List.fold_left instantiate (graph, IntMap.empty, []) (List.rev params) in
       unify_collect graph @@ List.map2 (fun a b -> unify graph a b) arg1 params
    | Term term, Call (OneOf options, args) ->
       let optf op = unify graph (Term term) (Call(op, args)) in
       let unification = List.map optf options in
       (match List.partition (function | Success _ -> true | _ -> false) unification with
        | [], _ -> Fail (Printf.sprintf "could not type %s, %s"
                          (cons_to_string @@ Term term)
                          (cons_to_string @@ Call (OneOf options, args)))
       | [single], _ -> single
       | _ -> Fail ("multiple options for term " ^ term))
    | a, b -> Fail (Printf.sprintf "unknown unification: %s <--> %s"
                      (cons_to_string a)
                      (cons_to_string b))
  let rec step graph shelf term = function 
    | Type (name, params) as t ->
       1, replace_term term t graph, shelf |> replace_term term t |> shelve term t
    | Term n as t ->
       1, replace_term term t graph, shelve term t shelf
    | AllOf(n :: []) | OneOf (n :: []) -> step graph shelf term n
    (* | OneOf n as t ->
     *    1, graph |> replace_term term t, shelve term t shelf *)
    | AllOf(Type _ as ty :: rest) ->
       let uni = unify_collect graph @@ List.map (unify graph ty) rest in
       (match uni with
        | Fail s -> failwith s
        | Success (graph, n) ->
           let graph = {graph with constraints = TermMap.remove term graph.constraints } in
           let graph = List.fold_left (fun gg (t, c) -> constrain gg t c) graph n in
           1, graph, shelve term ty shelf)
    | AllOf(Term n as nterm :: rest) ->
       (* when a == b && other things, b &= other things and get rid of a *)
       (* replace all references to term with n *)
       let graph = replace_term term nterm graph in
       (* add remaining constraints to n *)
       let update_n_term x = Some (addcons (AllOf rest) x) in
       let constraints = TermMap.update {name=n} update_n_term graph.constraints in
       1, {graph with constraints=constraints}, shelve term nterm shelf
    | Call(Type("Func", params), args) as call ->
       (match unify graph (Term term.name) call with
        | Fail s -> failwith ("type error:" ^ s)
        | Success (graph, items) ->
           let graph = {graph with constraints = TermMap.remove term graph.constraints} in
           let graph = List.fold_left (fun gg (t, c) -> constrain gg t c) graph items in
           1, graph, shelf)
    | Call(OneOf list, args) as call ->
       (match unify graph (Term term.name) call with
        | Fail s -> failwith ("type error:" ^ s)
        | Success (graph, items) ->
           let graph = {graph with constraints = TermMap.remove term graph.constraints} in
           let graph = List.fold_left (fun gg (t, c) -> constrain gg t c) graph items in
           1, graph, shelf)
    (* | Call _ -> *) 
    | cons ->
       Printf.printf "skipping: %s :: %s\n" term.name (cons_to_string cons);
       0, graph, shelf

  let finalize graph dependents =
    let rec find_type term =
      match TermMap.find_opt term dependents.constraints with
      | Some(Type _ as t) -> Some t
      | Some(Term m) -> find_type {name=m}
      | _ -> None in
    let rec update_cons = function
      | Term n -> (match find_type {name=n} with | None -> Term n | Some x -> x)
      | Type(a, ap) -> Type(a, List.map update_cons ap)
      | x -> x in
    {dependents with constraints = TermMap.map update_cons dependents.constraints }
  let solve graph =
    (* "Solving" a graph consists of iteratively removing constraints:
       1. term :: type foo can be removed via substitution
       2. term :: Term foo can be removed via substitution
       3: term :: Call(Type("Func", params), args) can be removed via
            pairwise reduction of params against (args @ [ term]) *)    
    let rec solve_step n (reduced:graph) (dependent:graph) =
      (* let merge term a b = match a, b with | a, b -> Some (AllOf [a; b]) in *)
      let step_once term cons (i, g1, g2) = step g1 g2 term cons in
      if config.debug then
        (Printf.printf "------------------------------------------------------------\n";
         showColor (5, 3, 3) reduced;
         showColor (2, 5, 3) dependent);
      match TermMap.fold step_once reduced.constraints (0, reduced, dependent) with
      | 0, a, b -> (0, a, b)
      | n, a, b -> solve_step (n - 1) a b
    in
    let i, g1, g2 = solve_step 10 graph empty
    in finalize g1 g2
      (* match TermMap.choose_opt reduced.constraints with
     *   | None -> finalize reduced dependent
     *   | Some(term, cons) ->
     *      let m, reduced, dependent = step reduced dependent term cons in
     *      (match n with
     *       | 0 -> finalize reduced dependent
     *       | _ -> solve_step (n - 1) reduced dependent)
     * in solve_step 10 graph empty *)
  end
