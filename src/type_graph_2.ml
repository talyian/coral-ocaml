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

module GraphF =
  functor (Node: sig
               type t
               val show: t -> string
               val empty: t
               val cmp: t -> t -> int
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

  type term = { name: string; value: Node.t }
  let term_name x= {name=x; value=Node.empty}
  module TermMap = Map.Make(struct type t = term let compare a b = compare a.name b.name end)
  module StringMap = Map.Make(String)
  module StringSet = Set.Make(String)
  module IntMap = Map.Make(struct type t = int let compare = compare end)

  type graph = {
      parent: graph option;
      terms: term StringMap.t;
      constraints: cons TermMap.t;
  }
  let empty = {terms=StringMap.empty; constraints=TermMap.empty; parent=None}
  let childOf parent = {empty with parent=Some(parent)}

  let showColor rgb graph =
    graph.constraints
    |> TermMap.iter (fun term cons ->
           Printf.printf "%s :: %s\n"
             (Ansicolor.as_rgb rgb @@ Printf.sprintf "%20s" term.name)
             (cons_to_string cons))
  let show = showColor (3,4,5)

  let rec findTerm graph name =
    match StringMap.find_opt name graph.terms with
    | Some(n) -> Some(n)
    | None ->
       match graph.parent with
       | Some(p) -> findTerm p name
       | None -> None

  let rec findTermByValue graph node =
    match graph.terms
          |> StringMap.bindings
          |> List.find_opt (function
             | name, {value=n} when Node.cmp n node = 0 && n <> Node.empty -> true
             | _ -> false) with
    | None ->
       showColor (4, 0, 0) graph;
       failwith (Printf.sprintf "not found: (%s)" (Node.show node))
    | Some(a, b) -> b
  let addTerm graph name node =
    let rec add_loop i =
      let newname = match i with
        | 0 -> name
        | n -> name ^ "." ^ string_of_int n in
      match findTerm graph newname with
      | Some(_) -> add_loop (i + 1)
      | None ->
         let term = {name=newname;value=node} in
         term, {graph with terms=StringMap.add newname term graph.terms} in
    add_loop 0

  (* After we create a new allof or oneof constraint,
   * we need to simplify it to keep it from exploding in size *)
  let rec simplify term = function
    | (OneOf [n]) | (AllOf [n]) -> simplify term n
    | OneOf list ->
       (match list |> List.filter ((<>) (Term term.name)) |> List.sort_uniq compare with
        | [n] -> simplify term n
        | list -> OneOf list)
    | AllOf list ->
       (match list |> List.filter ((<>) (Term term.name)) |> List.sort_uniq compare with
        | [n] -> simplify term n
        | list -> AllOf list)
    | n -> n

  let constrain graph term cons =
    match cons with
    | Term tt when tt = term.name -> graph
    | _ ->
       let update = function
         | None -> Some(cons)
         | Some (AllOf []) -> None
         | Some (OneOf []) -> None
         | Some(AllOf list) -> Some(simplify term @@ AllOf (cons :: list))
         | Some(x) -> Some(simplify term @@ AllOf [cons; x]) in
       { graph with constraints=TermMap.update term update graph.constraints}

  let remove_term (tt:term) graph =
    {graph with constraints = TermMap.remove tt graph.constraints}

  let replace_term_1 term replacement graph =
    (* Printf.printf "Replacing %s with %s\n" term.name (cons_to_string cons); *)
    let rec replace_func (tt:term) (tc:cons) =
      match tc with
      | Term s when s = term.name -> replacement
      | Type (n, p) -> Type (n, List.map (replace_func tt) p)
      | Call (callee, args) -> Call((replace_func tt) callee, List.map (replace_func tt) args)
      | OneOf p -> simplify tt @@ OneOf (p |> List.map (replace_func tt))
      | AllOf p -> simplify tt @@ AllOf (p |> List.map (replace_func tt))
      | _ as f -> f in
    {graph with constraints = graph.constraints |> TermMap.mapi replace_func}
  let replace_term term cons graph = graph
    |> replace_term_1 term cons
    |> remove_term term

  (* when replacing terms in "inactive", make them active *)
  let activate_replace_term term cons active inactive =
    let rec dependsOn (subject:term) (tt:term) = function
      | Term s when s = subject.name -> true
      | Type (n, p) -> List.exists (dependsOn subject term) p
      | Call (a, b) -> List.exists (dependsOn subject term) (a :: b)
      | OneOf p -> List.exists (dependsOn subject term) p
      | AllOf p -> List.exists (dependsOn subject term) p
      | _ -> false in
    let inactive_1, inactive_2 = TermMap.partition (dependsOn term) inactive.constraints in
    inactive_1 |> TermMap.iter (fun tt tc  ->
                      Printf.printf "Activating %s :: %s because of %s\n"
                        tt.name
                        (cons_to_string tc)
                        term.name)
    ;
    let active =
      let combine key a b = Some(AllOf [a; b]) in
      let constraints = TermMap.union combine inactive_1 active.constraints in
      {active with constraints = constraints} in
    let inactive = {inactive with constraints = inactive_2} in
    replace_term term cons active, replace_term term cons inactive

  (* add : cons -> cons option -> cons
     Adds two constraints, merging AllOfs to avoid nesting *)
  let addcons cons foo = match cons, foo with
    | cc, None -> cc
    | AllOf y, Some(AllOf x) -> AllOf (y @ x)
    | cons, Some(AllOf x) -> AllOf (cons :: x)
    | AllOf y, Some(cc) -> AllOf (cc :: y)
    | a, Some(b) -> AllOf [a; b]

  let shelve term cons graph =
    let updater x = Some(simplify term @@ addcons cons x) in
    {graph with constraints = TermMap.update term updater graph.constraints}

  type unification =
    | Fail of string
    | Defer of (term * cons) list
    | Success of graph * (term * cons) list

  let unify_collect graph =
    List.fold_left (fun a b ->
      match a, b with
      | Fail s, Fail t -> Fail (s ^ "; " ^ t)
      | (Fail s, _) | (_, Fail s) -> Fail s
      | (Defer y, Success (g, x)) | (Success (g, x), Defer y) -> Success(g, y @ x)
      | Defer x, Defer y -> Defer (x @ y)
      | Success (gg, s), Success (hh, t) -> Success (graph, s @ t)) (Success (graph, []))

(*
   The solving function -
   At each step we can evaluate an existing constraint and decide:
    [A] it is resolvable into a set of *simpler* constraints
    [B] it leads to a logical error
    [C] not enough info
*)
  let rec unify graph tt cons1 cons2 = match cons1, cons2 with
    | Type (a, ap), Type(b, bp) ->
       if a = b then
         unify_collect graph @@ List.map2 (unify graph tt) ap bp
       else
         Fail (Printf.sprintf "mismatch: %s, %s" a b)
    | Term x, Term y -> Success (graph, [term_name x, Term y])
    | (Term x, (Type _ as y)) | (Type _ as y, Term x) -> Success (graph, [term_name x, y])
    | (Type _ as cons1, Call (Type("Func", params), args))
    | (Term _ as cons1, Call (Type("Func", params), args)) ->
       let arg1 = args @ [cons1] in
       let instantiate (gg, map, pp) = (function
           | Free n -> (match IntMap.find_opt n map with
               | None ->
                  let nt, gg = addTerm gg "free" Node.empty in
                  gg, IntMap.add n nt map, (Term nt.name) :: pp
               | Some(t) -> gg, map, Term t.name :: pp)
           | n -> gg, map, n :: pp) in
       let graph, _, params =
         List.fold_left instantiate (graph, IntMap.empty, []) (List.rev params) in

       (* handle varargs *)
       let arg1, params =
         if List.exists (function | Type ("...", _) -> true | _ -> false) params then
           let new_args = [cons1] in
           let new_params = [List.hd @@ List.rev params] in
           new_args, new_params
         else
           arg1, params in

       if List.length arg1 <> List.length params then
         Fail (Printf.sprintf "type mismatch %d %d"
                 (List.length arg1 )
                 (List.length params))
       else
         unify_collect graph @@ List.map2 (fun a b -> unify graph tt a b) arg1 params
    | (Type _ as cons1, (Call (OneOf options, args) as call))
    | (Term _ as cons1, (Call (OneOf options, args) as call)) ->
       let optf op = unify graph tt cons1 (Call(op, args)) in
       let unification = List.map optf options in
       (match List.partition (function | Success _ -> true | _ -> false) unification with
        | [], _ -> Fail (Printf.sprintf "could not type %s, %s"
                          (cons_to_string @@ cons1)
                          (cons_to_string @@ Call (OneOf options, args)))
        | [single], _ -> single
        | _ -> Printf.printf "multiple options for term %s\n" tt.name;
               Defer [tt, cons1; tt, call])
    | a, b ->
       Printf.printf "Deferring: %s <> %s\n" (cons_to_string a) (cons_to_string b);
       Defer [tt, a; tt, b]

  let rec step graph shelf term cons =
    Printf.printf "step: %s :: %s\n" term.name (cons_to_string cons);
    let show_pair tt tc =
      Printf.printf "Adding: %s :: %s\n" tt.name (cons_to_string tc) in
    let handle_unify_result =function
      | Fail s -> failwith s
      | Success (graph, items) ->
         let graph = {graph with constraints = TermMap.remove term graph.constraints} in
         let graph = List.fold_left (fun gg (t, c) ->
                         show_pair t c;
                         constrain gg t c) graph items in
         1, graph, shelf
      | Defer (items) ->
         let graph = List.fold_left (fun gg (t, c) -> constrain gg t c) graph items in
         0, graph, shelf in

    match cons with
    | Type (name, params) as t ->
       let gg, ss = activate_replace_term term t graph shelf in
       1, gg, (shelve term t ss)
    | Term n as t ->
       let gg, ss = activate_replace_term term t graph shelf in
       1, gg, (shelve term t ss)
    | AllOf(n :: []) | OneOf (n :: []) -> step graph shelf term n
    | OneOf n as t ->
       1, graph |> replace_term term t, shelve term t shelf
    | AllOf(Type _ as ty :: rest) ->
       let uni = unify_collect graph @@ List.map (unify graph term ty) rest in
       handle_unify_result uni
    | AllOf(Term n as nterm :: rest) ->
       (* when a == b && other things, b &= other things and get rid of a *)
       (* replace all references to term with n *)
       let graph = replace_term term nterm graph in
       (* add remaining constraints to n *)
       let update_n_term x = Some (simplify term @@ addcons (AllOf rest) x) in
       let constraints = TermMap.update (term_name n) update_n_term graph.constraints in
       1, {graph with constraints=constraints}, shelve term nterm shelf
    | cons -> handle_unify_result @@ unify graph term (Term term.name) cons

  let finalize graph dependents =
    let rec find_type terms term =
      match TermMap.find_opt term dependents.constraints with
      | Some(Type _ as t) -> Some t
      | Some(Term m) ->
         (match StringSet.find_opt m terms with
          | None -> find_type (StringSet.add m terms) (term_name m)
          | Some(m) -> Some (Term m))
      | _ -> None in
    let rec update_cons = function
      | Term n -> (match find_type StringSet.empty (term_name n) with | None -> Term n | Some x -> x)
      | Type(a, ap) -> Type(a, List.map update_cons ap)
      | x -> x in
    {dependents with constraints = TermMap.map update_cons dependents.constraints }
  let solve graph =
    (* "Solving" a graph consists of iteratively removing constraints:
       1. term :: type foo can be removed via substitution
       2. term :: Term foo can be removed via substitution
       3: term :: Call(Type("Func", params), args) can be removed via
            pairwise reduction of params against (args @ [ term]) *)

    (* solve_step iterates through all the active terms once *)
    let rec solve_step n (reduced:graph) (dependent:graph) =
      (* let merge term a b = match a, b with | a, b -> Some (AllOf [a; b]) in *)
      let step_once term cons (i, g1, g2) =
        let cons = TermMap.find term g1.constraints in
        let j, g1, g2 = step g1 g2 term cons in
        (* if config.debug then (
         *   Printf.printf "step: %s = %s\n" term.name (cons_to_string cons);
         *   showColor (5, 5, 5) g1;
         *   showColor (5, 2, 5) g2); *)
        i + j, g1, g2 in
      if config.debug then begin
        (Printf.printf "------------------------------------------------------------\n";
         showColor (5, 3, 3) reduced;
         showColor (2, 5, 3) dependent);
        Printf.printf "[%d]\n" n;
        flush stdout;
      end;
      match TermMap.fold step_once reduced.constraints (0, reduced, dependent) with
      | 0, a, b -> (0, a, b)
      | other, a, b ->
         Printf.printf "ct: [%d]\n" other;
         if n > 0 then solve_step (n - 1) a b else -1, a, b
    in
    let i, g1, g2 = solve_step 10 graph empty
    in finalize g1 g2
end
