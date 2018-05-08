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
    | Call of cons * cons list * cons
    | OneOf of cons list
    | AllOf of cons list
    | Member of cons * string * cons
  let rec cons_to_string = function
    | Free n -> "τ" ^ string_of_int n
    | Term s -> "@" ^ s
    | Type (n, []) -> n
    | Type (n, p) ->
       n ^ "[" ^ (String.concat ", " (List.map cons_to_string p)) ^ "]"
    | Call (a, p, overload) ->
       cons_to_string a ^ "(" ^ (String.concat ", " (List.map cons_to_string p)) ^ ")"
    | OneOf p -> "(" ^ (String.concat "|" (List.map cons_to_string p)) ^ ")"
    | AllOf p -> "(" ^ (String.concat " &\n\t " (List.map cons_to_string p)) ^ ")"
    | Member(a, m, index) ->
       let str_base = cons_to_string a in
       let str_index =cons_to_string index in
       "(" ^ str_base ^ "[" ^ str_index ^ "])->" ^ m

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
    let uniq list =
      let remove x = function
        | n :: xs -> if x = n then xs else n :: xs
        | [] -> [] in
      match list with
      | [] -> []
      | x :: xs -> x :: remove x xs in
    let rec merge_ones = function
      | OneOf x :: rest -> merge_ones x @ merge_ones rest
      | x :: rest -> x :: merge_ones rest
      | [] -> [] in
    (* let rec merge_alls tt = function
     *   | AllOf x :: rest -> merge_alls (x @ tt) rest
     *   | x :: rest -> merge_alls (x :: tt) rest
     *   | [] -> (match tt with | [n] -> simplify term n | x -> [x) in
     * let merge_alls x = List.rev @@ merge_alls [] x  in *)
    let rec merge_alls = function
      | AllOf x :: rest -> merge_alls x @ merge_alls rest
      | x :: rest -> x :: merge_alls rest
      | [] -> [] in
    function
    | (OneOf [n]) | (AllOf [n]) -> simplify term n
    | OneOf list ->
       let clean_list = uniq @@ noterm @@ list in
       let out = merge_ones @@ List.map (simplify term) clean_list in
       OneOf out
    | AllOf list ->
       let clean_list = list |> noterm |> uniq in
       let out = merge_alls @@ List.map (simplify term) clean_list in
       AllOf out
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
      | Call (a, b, o) ->
         let c, p2 = recurse a in
         let d, p3 = recurseList b in
         c + d, Call(p2, p3, o)
      | OneOf p -> let c, p2 = recurseList p in c, simplify subject @@ OneOf p2
      | AllOf p -> let c, p2 = recurseList p in c, simplify subject @@ AllOf p2
      | Member (base, path, index) ->
         let count, newbase = recurse base in
         count, Member(newbase, path, index)
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

  let rec unify_folder (res, graph, tt) (a, b) =
    match res with
    | Fail s -> Fail s, graph, tt
    | Defer y -> (
      match unify graph tt a b with
      | Fail s -> Fail s, graph, tt
      | Defer z -> Defer (y @ z), graph, tt
      | Success(hh, z) -> Success(hh, y @ z), hh, tt)
    | Success (gg, prev) -> (
      match unify gg tt a b with
      | Fail s -> Fail s, graph, tt
      | Defer y -> Success(gg, y @ prev), gg, tt
      | Success(hh, y) -> Success(hh, y @ prev), hh, tt)

  and unify_zip graph tt ap bp =
    let zip = List.map2 (fun a b -> a, b) ap bp in
    let initial = Success (graph, []), graph, tt in
    let result, graph, _ = List.fold_left unify_folder initial zip in
    result

  (* let rec unify_items graph tt items =
   *   function
   *   | item *)
  (* Nullify takes a graph, term, and constraint
     and returns either a success|defer|fail judgement
     that simplifies the constraint *)
  and nullify graph tt = function
    (* | Member(Type (typename, _), member, Term out_index) -> *)
    | Call(Type("Func", params), args, o) as call -> unify graph tt (Term tt.name) call
    | OneOf [] | AllOf [] -> Success (graph, [])
    | OneOf [a] -> Success (graph, [tt, a])
    | AllOf [a] -> Success (graph, [tt, a])
    | cons -> Defer [tt, cons]
(*
   The solving function -
   At each step we can evaluate an existing constraint and decide:
    [A] it is resolvable into a set of *simpler* constraints
    [B] it leads to a logical error
    [C] not enough info
*)
  and unify graph tt cons1 cons2 = match cons1, cons2 with
    | Type (name_a, ap), Type(name_b, bp) ->
       if name_a <> name_b then
         Fail (Printf.sprintf "mismatch: %s, %s" name_a name_b)
       else
         unify_zip graph tt ap bp
    | Term x, Term y ->
       Success (graph, [term_by_name [graph] x, Term y])
    | (Term x, (Type _ as y)) | (Type _ as y, Term x) ->
       Success (graph, [term_by_name [graph] x, y])

    | (Type _ as cons1, Call (Type("Func", params), args, overload))
    | (Term _ as cons1, Call (Type("Func", params), args, overload)) ->
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
         unify_zip graph tt arg1 params
    | (Type _ as cons1, (Call (OneOf options, args, overload) as call))
    | (Term _ as cons1, (Call (OneOf options, args, overload) as call)) ->
       let optf op = unify graph tt cons1 (Call(op, args, overload)) in
       let unification = List.map optf options in
       let uni_indexed = List.mapi (fun i x -> i, x) unification in
       let matches = function | n, Fail _ -> false | _ -> true in
       (match List.partition matches uni_indexed with
        | [], _ -> Fail (Printf.sprintf "could not type %s, %s"
                          (cons_to_string @@ cons1)
                          (cons_to_string @@ Call (OneOf options, args, overload)))
        | [i, Success (gg, items)], _ ->
           let overload_type = (Type ("Overload", [Type (string_of_int i, [])])) in
           (match unify gg tt overload overload_type with
            | Fail s -> failwith s
            | Success (gg1, x) -> Success (gg1, x @ items)
            | Defer x -> Success (gg, x @ items)) (* not sure what this means *)
        | _ -> Defer [tt, cons1; tt, call])
    | (Type _ as y, Member(Type (typename, []), membername, index_cons)) ->
       let infokey = "(MemberType) " ^ typename ^ "::" ^ membername in
       let indexkey = "(MemberIndex) " ^ typename ^ "::" ^ membername in
       (match findTerm graph infokey, findTerm graph indexkey with
        | Some(info), Some(idx_ordinal_term) ->
           let graph =
             match index_cons with
             | Term index_term ->
                let indexterm = Type ("Index", [Term idx_ordinal_term.name]) in
                let graph = constrain graph (term_by_name [graph] index_term) indexterm in
                {graph with active_terms=TermSet.add idx_ordinal_term graph.active_terms}
             | _ -> failwith "unknown index holder" in
           unify graph tt (Term info.name) y
        | _ -> failwith ("member lookup failed on type " ^ infokey))
    | (Term tname, Member(Type (typename, []), membername, index_cons)) ->
       let infokey = "(MemberType) " ^ typename ^ "::" ^ membername in
       let indexkey = "(MemberIndex) " ^ typename ^ "::" ^ membername in
       (match findTerm graph infokey, findTerm graph indexkey with
        | Some(t), Some(idx_ordinal_term) ->
           let term = term_by_name [graph] tname in
           let graph =
             match index_cons with
             | Term index_term ->
                let indexterm = Type ("Index", [Term idx_ordinal_term.name]) in
                constrain graph (term_by_name [graph] index_term) indexterm
             | _ -> failwith "unknown index holder" in
           let graph = {graph with active_terms= TermSet.add idx_ordinal_term graph.active_terms}
           in
           Success (graph, [term, Term t.name])
        | _ -> failwith ("member lookup failed on type " ^ infokey))
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
       (* to simplify a N-ary &&-expression, we can either find a simple constraint
          like term/type and unify it against the remaining to get a (N-1)-ary &&-expr
          OR
          we can nullify each individual term in the expression *)
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
        | _ ->
           let unifold graph cons =
             match unify graph term (Term term.name) cons with
             | Fail s -> failwith s
             | Success (gg, items) -> List.fold_left (fun gg (t, c) -> constrain gg t c) gg items
             | Defer items -> graph in
           let graph = List.fold_left unifold graph items in
           1, graph
       )
    | cons ->
       handle_unify_result @@ unify graph term (Term term.name) cons

  let finalize graph = graph
  let solve graph =
    (* "Solving" a graph consists of iteratively removing constraints:
       1. term :: type foo can be removed via substitution
       2. term :: Term foo can be removed via substitution
       3: term :: Call(Type("Func", params), args) can be removed via
            pairwise reduction of params against (args @ [ term]) *)
    (* solve_step iterates through all the active terms once *)
    let rec solve_step n graph =
      if config.debug then
        begin
          Printf.printf "------------------------------------------------------------\n";
          showColor (5, 2, 1) graph;
          Printf.printf "[%d]\n" n;
          flush stdout;
        end;
      let step_once term (i, g1) =
        match TermMap.find_opt term g1.constraints with
        | None -> 0, g1
        | Some(cons) -> let j, g1 = step g1 term cons in i + j, g1 in
      match TermSet.fold step_once graph.active_terms (0, graph) with
      | 0, b -> (0, b)
      | other, b ->
         if config.debug then Printf.printf "ct: [%d]\n" other;
         if n > 0 then solve_step (n - 1) b else -1, b in
    (* 10 steps is an arbitrary limit *)
    let i, g1 = solve_step 10 graph
    in finalize g1

end
