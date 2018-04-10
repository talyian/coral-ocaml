(* An AST analysis pass that uses the Type_graph engine to infer unknown types,
   then populates missing type annotations in the AST. *)
open Ast
open Type_graph

let rec createGraph (g:graph) = function
  | Module list ->
     ignore (List.map (createGraph g) list);
     None
  |  Block list ->
     let terms = (List.map (createGraph g) list) in
     List.rev terms |> List.hd
  | Func(name, ret, params, body) as f ->
     (* let g = List.fold_left (fun g (p:defNode) -> (p.name, Def p) :: g) g params in *)
     (* let g = (name, f) :: g in *)
     let t = Graph.addTerm g name f in
     let runp (p:defNode) = let pterm = Graph.addTerm g p.name (Def p) in Term pterm in
     let ttparams = List.map runp params in
     let body_term = createGraph g body in
     (match body_term with
     | None -> ()
     | Some(bt) -> Graph.addCons g t (Type ("Func", ttparams @ [Term bt]))
     );
     Some t
  | If (cond, i, e) ->
     let cond_term = createGraph g cond in
     let if_term = createGraph g i in
     let else_term = createGraph g e in
     let term = Graph.addTerm g "if" i in
     (match if_term, else_term with
      | Some(it), Some(et) ->
         Graph.addCons g term (Union(Term it, Term et));
         ignore cond_term;
         Some term
      | _ -> failwith "invalid terms in if type analysis")
  | Let (var, expr) as x ->
     let expr_term = createGraph g expr in
     let term = Graph.addTerm g var.name x in
     (match expr_term with
      | Some(e) -> Graph.addCons g term (Term e); Some term
      | _ -> failwith "invalid term in let type analysis")
  | IntLiteral i as v ->
     let t = Graph.addTerm g ("i" ^ i) v in
     let cons = (Type ("Int64", [])) in
     Graph.addCons g t cons;
     Some t
  | StringLiteral i as v ->
     let t = Graph.addTerm g ("s" ^ i) v in
     let cons = (Type ("String", [])) in
     Graph.addCons g t cons; Some t
  | FloatLiteral i as v ->
     let t = Graph.addTerm g ("f" ^ i) v in
     let cons = (Type ("Float64", [])) in
     Graph.addCons g t cons; Some t
  | Return e -> createGraph g e
  | Binop (op, lhs, rhs) as x->
     let rterm = createGraph g rhs in
     let lterm = createGraph g lhs in
     let t = Graph.addTerm g op x in
     (match lterm, rterm with
      | Some(lt), Some(rt) ->
         (* TODO: This probably isn't completely correct *)
         let id2 = let free = Free 0 in Type ("Func", [free; free; free]) in
         let ideq = let free = Free 0 in Type ("Func", [free; free; Type ("Bool", [])]) in
         let cons = (
             match op with
             | "+" -> id2 | "-" -> id2 | "*" -> id2 | "/" -> id2 | "%" -> id2
             | "<" -> ideq | ">" -> ideq | "=" -> ideq
             | "<=" -> ideq | ">=" -> ideq | "!=" -> ideq
             | _ -> id2) in
         Graph.addCons g t (Call ((cons, [Term lt; Term rt])));
      | _ -> failwith "invalid type term in binop");
     Some t
  | Tuple list as t ->
     let terms = (List.fold_left (fun out x ->
                      match createGraph g x with
                      | Some(n) -> Term n :: out
                      | None -> failwith "invalid type term in tuple") [] list) in
     let cons = Type ("Tuple", terms) in
     let tuple_term = Graph.addTerm g "tuple" t in
     Graph.addCons g tuple_term cons; Some tuple_term
  | Var v ->
     (match v.target with
      | None -> failwith ("unresolved reference " ^ v.name)
      | Some(expr) -> Some (Graph.findTermByExpr g expr)
     )
  | Call (callee, args) as x ->
     let terms = (List.fold_left (fun out x ->
                      match createGraph g x with
                      | Some(n) -> Term n :: out
                      | None -> failwith "invalid type term in call") [] args) in
     (match createGraph g callee with
      | None -> failwith "invalid callee term in call"
      | Some(callee_term) ->
         let term = Graph.addTerm g ("call." ^ callee_term.name) x in
         Graph.addCons g term (Call (Term callee_term, terms));
         Some term)
  | n -> Printf.printf "Resolving Type: %s\n" (nodeName n); None

let applySolution solution =
  let rec constraint_to_type = function
    | Type (name, cons) ->
       let cons2 = List.map constraint_to_type cons in
       let cons_v = List.fold_left (fun a -> function | None -> a | Some(n) -> n :: a) [] cons2 in
       Some(
           match List.length cons_v with
           | 0 -> Ast.Type name
           | n when n <> List.length cons2 -> failwith "mismatched type lengths"
           | n -> Parameterized (name, cons_v))
    | _ -> None in

  let rec applyType cons_type node =
    match node with
     | Func (name, Type(""), params, body) as f ->
        Printf.printf "func name %s returns %s\n" name (type_to_string cons_type);
        (match cons_type with
        | Parameterized ("Func", cons_params) ->
           let (ret :: ptypes) = List.rev cons_params in
           let newparams = List.map2 applyType ptypes (List.rev params |> List.map (fun v -> Def v)) in
           Func (name,
                 ret,
                 List.rev newparams |> List.map (function | Def v -> v),
                 body)
        | _ -> f)
     | Def v as d ->
        Printf.printf "def (%s) is typed %s\n" v.name (type_to_string cons_type);
        d
     | Let (v, value) ->
        Printf.printf "let (%s) is typed %s\n" v.name (type_to_string cons_type);
        Let(v, value)
     | Return v ->
        Printf.printf "return is typed %s\n" (type_to_string cons_type);
        Return v
     | n -> n
  in
  let apply = function
    | {contents=({node=Some(e)}, (Type _ as t))} ->
       (match constraint_to_type t with
        | None -> ()
        | Some(cons_type) -> ignore (applyType cons_type e))
    | _ -> () in
  List.iter apply solution.active_edges;
  0

let run m =
  let graph = Graph.create () in
  let term = createGraph graph m in ignore term;
  let solution = Solution.solve graph in
  Solution.show solution;
  applySolution solution;
  Ast.show m;
  m
