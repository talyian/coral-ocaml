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
     Graph.addCons g t cons; Some t
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
             | "+" -> id2
             | "-" -> id2
             | "*" -> id2
             | "/" -> id2
             | "%" -> id2
             | "<" -> ideq
             | ">" -> ideq
             | "=" -> ideq
             | "<=" -> ideq
             | ">=" -> ideq
             | "!=" -> ideq
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

let run m =
  let graph = Graph.create () in
  ignore (createGraph graph m);
  solve graph |> ignore;
  (* let solution = Graph.solve graph; *)
  m
