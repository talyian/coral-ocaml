(* An AST analysis pass that uses the Type_graph engine to infer unknown types,
   then populates missing type annotations in the AST. *)
open Ast
open Type_graph

let rec type_to_constraint = function
  | Ast.Free -> failwith "unhandled free term"
  | Ast.Type "Tuple" -> Type_graph.Type ("Void", [])                         
  | Ast.Type s -> Type_graph.Type (s, [])
  | Ast.Parameterized ("Tuple", []) -> Type_graph.Type ("Void", [])
  | Ast.Parameterized (s, p) -> Type_graph.Type (s, List.map type_to_constraint p)
  | Ast.Dotted items -> failwith "unhandled dotted type"
                             
let rec createGraph (g:graph) node =
  (* Printf.printf "Node: ";
   * (Ast.show node);
   * flush stdout; *)
  match node with
  | Module list ->
     ignore (List.map (createGraph g) list);
     None
  |  Block list ->
     let terms = (List.map (createGraph g) list) in
     List.rev terms |> List.hd
  | Func{name=name; ret_type=ret_type; params=params; body=body} as f ->
     (* let g = List.fold_left (fun g (p:defNode) -> (p.name, Def p) :: g) g params in *)
     (* let g = (name, f) :: g in *)
     let t = Graph.addTerm g name f in
     let runp (p:defNode) = let pterm = Graph.addTerm g p.name (Def p) in Term pterm in
     let ttparams = List.map runp params in
     let ret_term = (match body with
      | Empty -> Graph.addOptionalTerm g (t.name ^ ".ret") None
      | _ -> match createGraph g body with
             | None -> Graph.addOptionalTerm g (t.name ^ ".ret") None
             | Some(bt) -> bt
                    ) in
     Graph.addCons g t (Type ("Func", ttparams @ [Term ret_term]));
     Graph.addCons g ret_term (type_to_constraint ret_type);
     Some t
  | If (cond, i, e) ->
     let cond_term = createGraph g cond in
     let if_term = createGraph g i in
     let else_term = createGraph g e in
     let term = Graph.addTerm g "if" i in
     (match if_term, else_term with
      | Some(it), Some(et) ->
         Graph.addCons g term (Union [Term it; Term et]);
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
     let t = Graph.addTerm g ("s" ^ string_escape i) v in
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
      | _ ->
         (match lterm, rterm with
          | _, None -> Printf.printf "rterm is none\n"
          | None, _ ->
             Printf.printf "lterm is none\n"
            ; Ast.show lhs
          | _ -> ());
         failwith "invalid type term in binop");
     Some t
  | Tuple list as t ->
     let terms = (List.fold_left (fun out x ->
                      match createGraph g x with
                      | Some(n) -> Term n :: out
                      | None -> failwith "invalid type term in tuple") [] list) in
     let cons = (match terms with
                 | [] -> Type("Void", [])
                 | terms -> Type ("Tuple", terms)) in
     let tuple_term = Graph.addTerm g "tuple" t in
     Graph.addCons g tuple_term cons; Some tuple_term
  | Var v ->
     (match v.target with
      | None -> failwith ("unresolved reference " ^ v.name)
      | Some(expr) -> Some(Graph.findTermByExpr g expr)
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
  | Multifunc (name, funcs) as mf ->
     let term = Graph.addTerm g name mf in
     let func_terms = funcs
                      |> List.map (fun f -> createGraph g (Func f))
                      |> List.map (function | Some n -> Term n | _ -> failwith "missing term")
     in
     Graph.addCons g term (Union func_terms);
     Some term
  | n -> Printf.printf "Type-resolving Type: %s\n" (nodeName n); None

let applySolution (solution:Solver.solution) =
  let rec constraint_to_type = function
    | Type (name, cons) ->
       let cons2 = List.map constraint_to_type cons in
       let cons_v = List.fold_left (fun a -> function | None -> a | Some(n) -> n :: a) [] cons2 in
       (match List.length cons_v with
        | 0 -> Some(Ast.Type name)
        | n when n <> List.length cons2 -> None
        | n -> Some(Parameterized (name, cons_v)))
    | _ -> None in

  let rec applyType cons_type node =
    match node with
     | Func func as f ->
        (* Printf.printf "func name %s returns %s\n" func.name (type_to_string cons_type); *)
        (match cons_type with
         | Parameterized ("Func", cons_params) ->
            (match List.rev cons_params with
             | [] -> failwith "???"
             | ret :: ptypes ->
                let newparams =
                  List.map2
                    applyType
                    ptypes
                    (List.rev func.params |> List.map (fun v -> Def v)) in
                func.ret_type <- ret;
                ignore newparams;
                f)
        | _ -> f)
     | Def v as d ->
        (* Printf.printf "def (%s) is typed %s\n" v.name (type_to_string cons_type); *)
        v.defType <- Some cons_type;
        d
     | Let (v, value) ->
        (* Printf.printf "let (%s) is typed %s\n" v.name (type_to_string cons_type); *)
        v.varType <- Some cons_type;
        Let(v, value)
     | Return v ->
        (* Printf.printf "return is typed %s\n" (type_to_string cons_type); *)
        Return v
     | n -> n
  in
  let apply = function
    | {term={node=Some(e)}; cons=(Type _ as t)} ->
       (match constraint_to_type t with
        | None -> ()
        | Some(cons_type) -> ignore (applyType cons_type e))
    | _ -> () in

  solution.Solver.dependent_terms
  |> TermMap.bindings
  |> List.map (fun (t, edges) -> EdgeSet.elements edges)
  |> List.concat
  |> List.iter (fun e -> apply e)

let run m =
  let graph = Graph.create () in
  let term = createGraph graph m in
  ignore term;
  let solution = Solver.init graph in
  let solution = Solver.solve solution in
  applySolution solution;
  m
