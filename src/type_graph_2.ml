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
  let config = { debug = false }
  type cons =
    | Free of int
    | Term of string
    | Type of string * cons list
    | Call of cons * cons list
    | OneOf of cons list
    | AllOf of cons list
    | Member of cons * string
  let rec cons_to_string = function
    | Free n -> "τ" ^ string_of_int n
    | Term s -> "@" ^ s
    | Type (n, []) -> n
    | Type (n, p) ->
       n ^ "[" ^ (String.concat ", " (List.map cons_to_string p)) ^ "]"
    | Call (a, p) ->
       cons_to_string a ^ "(" ^ (String.concat ", " (List.map cons_to_string p)) ^ ")"
    | OneOf p -> "(" ^ (String.concat "|" (List.map cons_to_string p)) ^ ")"
    | AllOf p -> "(" ^ (String.concat " & " (List.map cons_to_string p)) ^ ")"
    | Member(a, m) -> "(" ^ cons_to_string a ^ ")->" ^ m

  type term = { name: string; value: Node.t }
  module TermMap = Map.Make(struct type t = term let compare a b = compare a.name b.name end)
  module StringMap = Map.Make(String)
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  module TermSet = Set.Make(struct type t = term let compare a b = compare a.name b.name end)

  type graph = {
      parent: graph option;
      terms: term StringMap.t;
      active_terms: TermSet.t;
      constraints: cons TermMap.t;
  }
  let empty = {terms=StringMap.empty;
               constraints=TermMap.empty;
               active_terms=TermSet.empty;
               parent=None}
  let childOf parent = {empty with parent=Some(parent)}

  let showColor rgb graph =
    let loop term cons =
      let rgb = if TermSet.mem term graph.active_terms then rgb else (2, 2, 2) in
      Printf.printf "%s :: %s\n"
        (Ansicolor.as_rgb rgb @@ Printf.sprintf "%20s" term.name)
        (cons_to_string cons) in
    TermMap.iter loop graph.constraints
  let show = showColor (3,4,5)

  let rec findTerm graph name =
    match StringMap.find_opt name graph.terms with
    | Some(n) -> Some(n)
    | None ->
       match graph.parent with
       | Some(p) -> findTerm p name
       | None -> None

  let rec term_by_name graphs x =
    match graphs with
    | [] -> failwith ("term_by_name: " ^  x)
    | graph :: next ->
       match findTerm graph x with
       | Some(x) -> x
       | _ -> term_by_name next x

  let rec findTermByValue graph node =
    match graph.terms
          |> StringMap.bindings
          |> List.find_opt (function
             | name, {value=n} when Node.cmp n node = 0 && n <> Node.empty -> true
             | _ -> false) with
    | None ->
       showColor (3, 2, 2) graph;
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
  let rec simplify term =
    let noterm = List.filter ((<>) (Term term.name)) in
    let uniq = List.sort_uniq compare in
    let rec merge_ones tt = function
      | OneOf x :: rest -> merge_ones (x @ tt) rest
      | x :: rest -> merge_ones (x :: tt) rest
      | [] -> (match tt with | [n] -> simplify term n | x -> OneOf x) in
    let rec merge_alls tt = function
      | AllOf x :: rest -> merge_alls (x @ tt) rest
      | x :: rest -> merge_alls (x :: tt) rest
      | [] -> (match tt with | [n] -> simplify term n | x -> AllOf x) in
    function
    | (OneOf [n]) | (AllOf [n]) -> simplify term n
    | OneOf list -> list |> noterm |> uniq |> merge_ones []
    | AllOf list -> list |> noterm |> uniq |> merge_alls []
    | n -> n

  let constrain graph term cons =
    if config.debug then
      Printf.printf "%s: %s :: %s\n"
        (Ansicolor.as_color (Bold GREEN) "Add ") term.name (cons_to_string cons);
    match cons with
    | Term tt when tt = term.name -> graph
    | _ ->
       let update = function
         | None -> Some(cons)
         | Some (AllOf []) -> None
         | Some (OneOf []) -> None
         | Some(AllOf list) -> Some(simplify term @@ AllOf (cons :: list))
         | Some(x) -> Some(simplify term @@ AllOf [cons; x]) in
       { graph with
         constraints=TermMap.update term update graph.constraints;
         active_terms = TermSet.add term graph.active_terms}

  let remove_term (tt:term) graph =
    {graph with
      constraints = TermMap.remove tt graph.constraints;
      active_terms = TermSet.remove tt graph.active_terms}

  (* when replacing terms in "inactive", make them active *)
  let activate_replace_term term cons graph =
    (* Replaces subject->replacement in a given constraint,
       returning a new constraint and a replacement count. *)
    let rec doReplace subject replacement =
      let recurse x = doReplace subject replacement x in
      let rec recurseList = function
        | [] -> 0, []
        | x :: rest ->
           let c, x2 = recurse x in
           let d, x3 = recurseList rest in
           c + d, (x2 :: x3) in
      function
      | Term s when s = subject.name -> 1, replacement
      | Type (n, p) ->
         let c, p2 = recurseList p in c, Type(n, p2)
      | Call (a, b) ->
         let c, p2 = recurse a in
         let d, p3 = recurseList b in
         c + d, Call(p2, p3)
      | OneOf p -> let c, p2 = recurseList p in c, simplify subject @@ OneOf p2
      | AllOf p -> let c, p2 = recurseList p in c, simplify subject @@ AllOf p2
      | Member (base, path) ->
         let count, newbase = recurse base in
         count, Member(newbase, path)
      | n -> 0, n in

    let fold_replace key target (count, actives, constraints) =
      let c, new_cons = doReplace term cons target in
      let new_actives = match c with | 0 -> actives | n -> TermSet.add key actives in
      (count + c, new_actives, TermMap.add key new_cons constraints) in
    let cc, actives, constraints =
      TermMap.fold fold_replace graph.constraints (0, TermSet.empty, TermMap.empty) in
    cc, { graph with
      constraints=constraints;
      active_terms=TermSet.remove term @@ TermSet.union graph.active_terms actives
    }
    (* let inactive_1, inactive_2 = TermMap.partition (dependsOn term) inactive.constraints in
     * if config.debug then
     *   inactive_1 |> TermMap.iter (fun tt tc  ->
     *                   Printf.printf "Activating %s :: %s because of %s\n"
     *                     tt.name
     *                     (cons_to_string tc)
     *                     term.name)
     * ;
     * let active =
     *   let combine key a b = Some(AllOf [a; b]) in
     *   let constraints = TermMap.union combine inactive_1 active.constraints in
     *   {active with constraints = constraints} in
     * let inactive = {inactive with constraints = inactive_2} in
     * replace_term term cons active, replace_term term cons inactive *)

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
    | Term x, Term y -> Success (graph, [term_by_name [graph] x, Term y])
    | (Term x, (Type _ as y)) | (Type _ as y, Term x) -> Success (graph, [term_by_name [graph] x, y])
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
    | (Term tname, Member(Type (typename, []), membername)) ->
       let infokey = "(MemberType) " ^ typename ^ "::" ^ membername in
       (match findTerm graph infokey with
        | None -> failwith ("member lookup failed on type " ^ infokey)
        | Some(t) -> Success (graph, [term_by_name [graph] tname, Term t.name]))
    | a, b ->
       if config.debug then
         Printf.printf "Deferring: %s <> %s\n"
           (cons_to_string a)
           (cons_to_string b);
       Defer [tt, a; tt, b]

  let rec step graph term cons =
    if config.debug then Printf.printf "step: %s :: %s\n" term.name (cons_to_string cons);
    let handle_unify_result = function
      | Fail s -> failwith s
      | Success (graph, items) ->
         (* let graph = {graph with constraints = TermMap.remove term graph.constraints} in *)
         let graph = List.fold_left (fun gg (t, c) -> constrain gg t c) graph items in
         let newterms = List.map fst items |> TermSet.of_list in
         1, {graph with active_terms=TermSet.union graph.active_terms newterms}
      | Defer (items) ->
         (* let graph = List.fold_left (fun gg (t, c) -> constrain gg t c) graph items in *)
         0, graph in

    match cons with
    | Type (name, params) as t -> activate_replace_term term t graph
    | Term n as t -> activate_replace_term term t graph
    | AllOf(n :: []) | OneOf (n :: []) -> step graph term n
    | OneOf n as t -> activate_replace_term term (simplify term t) graph
    | AllOf items ->
       (match List.find_opt (function | Term _ -> true | Type _ -> true | _ -> false) items with
        | Some(Type(tt, tp) as cc) ->
           let rest = List.filter ((<>) cc) items in
           let graph = remove_term term graph in
           let c, graph = activate_replace_term term (Type(tt, tp)) graph in

           let cons_list = (Term term.name :: rest) in
           let unifold graph cons =
             match unify graph term (Type (tt, tp)) cons with
             | Fail s -> graph
             | Success (gg, items) -> List.fold_left (fun gg (t, c) -> constrain gg  t c) gg items
             | Defer items -> graph
           in
           let graph = List.fold_left unifold graph cons_list in
           c, graph
           (* handle_unify_result
            * @@ unify_collect graph
            * @@ List.map (unify graph term (Type(tt, tp))) (Term term.name :: rest) *)
        | Some(Term n as nterm) ->
           let rest = List.filter ((<>) nterm) items in
           let graph = remove_term term graph in
           let graph = constrain graph term nterm in
           (* when a == b && other things, b &= other things and get rid of a *)
           (* replace all references to term with n *)
           let c, graph = activate_replace_term term nterm graph in
           let n_term = term_by_name [graph] n in
           let folder graph cons = constrain graph n_term cons in
           let graph = List.fold_left folder (graph) rest in
           1, graph
        | _ -> 0, graph)
    (* | AllOf(Type (tt, tp) :: rest) -> *)
    (* | AllOf(Term n as nterm :: rest) -> *)
       (* add remaining constraints to n *)
       (* let update_n_term x = Some (simplify term @@ addcons (AllOf rest) x) in
        *
        * let constraints =
        *   TermMap.update
        *     (term_by_name [graph] n)
        *     update_n_term
        *     graph.constraints in
        * 1, {graph with constraints=constraints}, shelve term nterm *)
    | cons ->
       handle_unify_result @@ unify graph term (Term term.name) cons

  let finalize graph =
    (* let rec find_type terms term =
     *   match TermMap.find_opt term dependents.constraints with
     *   | Some(Type _ as t) -> Some t
     *   | Some(Term m) ->
     *      (match StringSet.find_opt m terms with
     *       | None -> find_type (StringSet.add m terms) (term_by_name [graph;dependents] m)
     *       | Some(m) -> Some (Term m))
     *   | _ -> None in
     * let rec update_cons = function
     *   | Term n -> (match find_type StringSet.empty (term_by_name [graph;dependents] n) with | None -> Term n | Some x -> x)
     *   | Type(a, ap) -> Type(a, List.map update_cons ap)
     *   | x -> x in
     * {dependents with constraints = TermMap.map update_cons dependents.constraints } *)
    graph
  let solve graph =
    (* "Solving" a graph consists of iteratively removing constraints:
       1. term :: type foo can be removed via substitution
       2. term :: Term foo can be removed via substitution
       3: term :: Call(Type("Func", params), args) can be removed via
            pairwise reduction of params against (args @ [ term]) *)

    let graph =
      {graph with active_terms = graph.constraints
                                 |> TermMap.bindings
                                 |> List.map fst
                                 |> TermSet.of_list} in
    (* solve_step iterates through all the active terms once *)
   let rec solve_step n graph =
      (* let merge term a b = match a, b with | a, b -> Some (AllOf [a; b]) in *)
      let step_once term (i, g1) =
        match TermMap.find_opt term g1.constraints with
        | None -> 0, g1
        | Some(cons) ->
           let j, g1 = step g1 term cons in
           (* if config.debug then (
            *   Printf.printf "step: %s = %s\n" term.name (cons_to_string cons);
            *   showColor (5, 5, 5) g1;
            *   showColor (5, 2, 5) g2); *)
           i + j, g1
      in
      if config.debug then
        begin
          Printf.printf "------------------------------------------------------------\n";
          showColor (5, 2, 1) graph;
          Printf.printf "[%d]\n" n;
          flush stdout;
        end;
      match TermSet.fold step_once graph.active_terms (0, graph) with
      | 0, b -> (0, b)
      | other, b ->
         if config.debug then Printf.printf "ct: [%d]\n" other;
         if n > 0 then solve_step (n - 1) b else -1, b
    in
     let i, g1 = solve_step 10 graph
    in finalize g1

end
