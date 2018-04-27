(* An AST analysis pass that uses the Type_graph engine to infer unknown types,
   then populates missing type annotations in the AST. *)
open Ast

module Graph = Type_graph_2.GraphF(struct
  type t = Ast.node
  let show n = Ast.nodeName n
  let empty = Ast.Empty
  let cmp = compare
end)

let rec type_to_constraint = function
  | Ast.Free -> failwith "unhandled free term"
  | Ast.Type "Tuple" -> Graph.Type ("Void", [])
  | Ast.Type s -> Graph.Type (s, [])
  | Ast.Parameterized ("Tuple", []) -> Graph.Type ("Void", [])
  | Ast.Parameterized (s, p) -> Graph.Type (s, List.map type_to_constraint p)
  | Ast.Dotted items -> failwith "unhandled dotted type"

let rec createGraph g node =
  match node with
  | Module list as m ->
     let fold (terms, g) a =
       let (t, g) = createGraph g a in
       (t::terms, g) in
     let terms, gg = List.fold_left fold ([], g) list in
     Graph.addTerm gg "module" m
  | Block list as block ->
     let blockterm, gg = Graph.addTerm g "block" block in
     (match list with
      | [] ->
         let gg = Graph.constrain gg blockterm (Graph.Type ("Void", []))in
         blockterm, gg
      | [n] -> createGraph gg n
      | list ->
         let fold (terms, g) a =
           let (t, g) = createGraph g a in
           (t::terms, g) in
         let terms, gg = List.fold_left fold ([], gg) list in
         let gg = Graph.constrain gg blockterm (Graph.Term (List.hd @@ List.rev terms).name) in
         blockterm, gg)
  | Func fdata as func ->
     let fold (terms, gg1) param =
       let (t, gg2) = createGraph gg1 param in
       (t::terms, gg2) in
     let terms, gg = List.fold_left fold ([], g) fdata.params in
     Graph.StringMap.iter
       (fun name (node:Graph.term) ->
         Printf.printf "* term: %s -> %s\n" name
           (Ast.nodeName node.value))
       gg.Graph.terms;
     Graph.showColor (0, 4, 0) gg;
     let terms = List.map (fun (x:Graph.term) -> Graph.Term x.name) @@ List.rev terms in
     let fterm, gg = Graph.addTerm gg ("fn." ^ fdata.name) func in
     let bodyTerm, gg = createGraph gg fdata.body in
     let retTerm, gg = Graph.addTerm gg (fdata.name ^ ".ret") Empty in
     let gg =
       match fdata.ret_type with
       | Ast.Type "" -> gg
       | n -> Graph.constrain gg retTerm (type_to_constraint fdata.ret_type) in
     let gg = Graph.constrain gg fterm (Graph.Type ("Func", terms @ [Graph.Term retTerm.name])) in
     fterm, gg
  | Def info as def ->
     let term, gg = Graph.addTerm g info.name def in
     (match info.defType with
      | None -> term, gg
      | Some(ctype) ->
         let gg = Graph.constrain gg term (type_to_constraint ctype) in
         term, gg)
  | If (cond, ifbody, elsebody) as ifnode ->
     let ifterm, gg = Graph.addTerm g "if" ifnode in
     let condterm, gg = createGraph gg cond in
     let gg = Graph.constrain gg condterm (Graph.Type ("Bool", [])) in
     let ifbodyterm, gg = createGraph gg ifbody in
     let elsebodyterm, gg = createGraph gg elsebody in
     let gg = Graph.constrain gg ifterm
                (Graph.OneOf [Graph.Term ifbodyterm.name; Graph.Term elsebodyterm.name]) in
     ifterm, gg
  | Return v as ret ->
     let term, gg = Graph.addTerm g "ret" ret in
     let val_term, gg = createGraph gg v.node in
     let gg = Graph.constrain gg term (Graph.Term val_term.name) in
     term, gg
  | IntLiteral i as inode ->
     let term, gg = Graph.addTerm g ("i" ^ i) inode in
     let gg = Graph.constrain gg term (Graph.Type ("Int32", [])) in
     term, gg
  | Binop (op, lhs, rhs) as binop ->
     let term, gg = Graph.addTerm g ("op." ^ op) binop in
     let lterm, gg = createGraph gg lhs in
     let rterm, gg = createGraph gg rhs in
     let gg =
       let callee = Graph.Term op in
       let args = [Graph.Term lterm.name; Graph.Term rterm.name] in
       Graph.constrain gg term (Graph.Call (callee, args)) in
     term, gg
  | Var info ->
     (match info.target with
      | None -> failwith "missing reference"
      | Some(target) ->
         let term = Graph.findTermByValue g target in
         term, g)
  | Tuple [] as tt ->
     let term, gg = Graph.addTerm g "x" tt in
     let gg = Graph.constrain gg term (Graph.Type ("Void", [])) in
     term, gg
  | Call (callee, args) as call->
     let calleeterm, gg = createGraph g callee in
     let fold (terms, g) a =
       let (t, g) = createGraph g a in
       (t::terms, g) in
     let argterms, gg = List.fold_left fold ([], gg) @@ List.rev args in
     let term, gg = Graph.addTerm gg ("call." ^ calleeterm.name) call in
     let gg =
       let c1 = Graph.Term calleeterm.name in
       let c2 = List.map (fun (t:Graph.term) -> Graph.Term t.name) argterms in
       Graph.constrain gg term (Graph.Call (c1, c2)) in
     term, gg
  | StringLiteral s as snode ->
     let term, gg = Graph.addTerm g ("s" ^ Ast.string_name_escape s) snode in
     let gg = Graph.constrain gg term (Graph.Type ("String", [])) in
     term, gg
  | x ->
     Printf.printf "createGraph: %s\n" (nodeName node);
     Graph.addTerm g (nodeName x) x
  (* |  Block list ->
   *    let terms = (List.map (createGraph g) list) in
   *    List.rev terms |> List.hd
   * | Func{name=name; ret_type=ret_type; params=params; body=body} as f ->
   *    (\* let g = List.fold_left (fun g (p:defNode) -> (p.name, Def p) :: g) g params in *\)
   *    (\* let g = (name, f) :: g in *\)
   *    let t = Graph.addTerm g name f in
   *    let runp (p:defNode) =
   *      let pterm, graph = Graph.addTerm g p.name (Def p) in
   *      Graph.Term pterm in
   *    let ttparams = List.map runp params in
   *    let ret_term = (match body with
   *     | Empty -> Graph.addOptionalTerm g (t.name ^ ".ret") None
   *     | _ -> match createGraph g body with
   *            | None -> Graph.addOptionalTerm g (t.name ^ ".ret") None
   *            | Some(bt) -> bt
   *                   ) in
   *    Graph.addCons g t (Type ("Func", ttparams @ [Term ret_term]));
   *    (match ret_type with
   *     | Type "" -> ()
   *     | rt -> Graph.addCons g ret_term (type_to_constraint rt));
   *    Some t
   * | If (cond, i, e) ->
   *    let cond_term = createGraph g cond in
   *    let if_term = createGraph g i in
   *    let else_term = createGraph g e in
   *    let term = Graph.addTerm g "if" i in
   *    (match if_term, else_term with
   *     | Some(it), Some(et) ->
   *        Graph.addCons g term (Union [Term it; Term et]);
   *        ignore cond_term;
   *        Some term
   *     | (None, Some(x))
   *     | (Some(x), None) -> Graph.addCons g term (Term x); Some term
   *     | _ ->
   *        Ast.show @@ If(cond, i, e);
   *        failwith "invalid terms in if type analysis")
   * | Let (var, expr) as x ->
   *    let expr_term = createGraph g expr in
   *    let term = Graph.addTerm g var.name x in
   *    (match var.varType with
   *     | Some(v) -> Graph.addCons g term (type_to_constraint v); Some term
   *     | None ->
   *        (match expr_term with
   *         | Some(e) -> Graph.addCons g term (Term e); Some term
   *         | _ -> failwith "invalid term in let type analysis"))
   * | IntLiteral i as v ->
   *    let t = Graph.addTerm g ("i" ^ i) v in
   *    let cons = (Type ("Int64", [])) in
   *    Graph.addCons g t cons;
   *    Some t
   * | StringLiteral i as v ->
   *    let i = match String.length i with
   *      | n when n < 15 -> i
   *      | _ -> String.sub i 0 13 ^ ".." in
   *    let t = Graph.addTerm g ("s" ^ string_name_escape i) v in
   *    let cons = (Type ("String", [])) in
   *    Graph.addCons g t cons; Some t
   * | FloatLiteral i as v ->
   *    let t = Graph.addTerm g ("f" ^ i) v in
   *    let cons = (Type ("Float64", [])) in
   *    Graph.addCons g t cons; Some t
   * | Return {node=e;coraltype=ctype} as r ->
   *    (match createGraph g e with
   *     | None -> None
   *     | Some(t) ->
   *        let term = Graph.addTerm g ("ret." ^ t.name) r in
   *        Graph.addCons g term (Term t);
   *        Some term)
   * | Binop (op, lhs, rhs) as x->
   *    let rterm = createGraph g rhs in
   *    let lterm = createGraph g lhs in
   *    let t = Graph.addTerm g op x in
   *    (match lterm, rterm with
   *     | Some(lt), Some(rt) ->
   *        (\* TODO: This probably isn't completely correct *\)
   *        let id2 = let free = Free 0 in Type ("Func", [free; free; free]) in
   *        let ideq = let free = Free 0 in Type ("Func", [free; free; Type ("Bool", [])]) in
   *        let cons = (
   *            match op with
   *            | "+" -> id2 | "-" -> id2 | "*" -> id2 | "/" -> id2 | "%" -> id2
   *            | "<" -> ideq | ">" -> ideq | "=" -> ideq
   *            | "<=" -> ideq | ">=" -> ideq | "!=" -> ideq
   *            | _ -> id2) in
   *        Graph.addCons g t (Call ((cons, [Term lt; Term rt])));
   *     | _ ->
   *        (match lterm, rterm with
   *         | _, None -> Printf.printf "rterm is none\n"
   *         | None, _ ->
   *            Printf.printf "lterm is none\n"
   *           ; Ast.show lhs
   *         | _ -> ());
   *        failwith "invalid type term in binop");
   *    Some t
   * | Tuple list as t ->
   *    let terms = (List.fold_left (fun out x ->
   *                     match createGraph g x with
   *                     | Some(n) -> Term n :: out
   *                     | None -> failwith "invalid type term in tuple") [] list) in
   *    let cons = (match terms with
   *                | [] -> Type("Void", [])
   *                | terms -> Type ("Tuple", terms)) in
   *    let tuple_term = Graph.addTerm g "tuple" t in
   *    Graph.addCons g tuple_term cons; Some tuple_term
   * | Var v ->
   *    (match v.target with
   *     | None -> failwith ("unresolved reference " ^ v.name)
   *     | Some(expr) -> Some(Graph.findTermByExpr g expr)
   *    )
   * | Call (callee, args) as x ->
   *    let terms = (List.fold_left (fun out x ->
   *                     match createGraph g x with
   *                     | Some(n) -> Term n :: out
   *                     | None -> failwith "invalid type term in call") [] args) in
   *    (match createGraph g callee with
   *     | None -> failwith "invalid callee term in call"
   *     | Some(callee_term) ->
   *        let term = Graph.addTerm g ("call." ^ callee_term.name) x in
   *        Graph.addCons g term (Call (Term callee_term, List.rev terms));
   *        Some term)
   * | Multifunc (name, funcs) as mf ->
   *    let term = Graph.addTerm g name mf in
   *    let func_terms = funcs
   *                     |> List.map (fun f -> createGraph g (Func f))
   *                     |> List.map (function | Some n -> Term n | _ -> failwith "missing term")
   *    in
   *    Graph.addCons g term (Union func_terms);
   *    Some term *)
  (* | n -> Printf.printf "Type-resolving Type: %s\n" (nodeName n);
   *        Graph.addTerm graph (nodeName n) n *)

let run m =
  let graph = Graph.empty in
  let term, graph = createGraph graph m in
  Graph.show graph;
  Printf.printf "hi\n";
  exit 0
  m
   (*
 * let applySolution (solution:Solver.solution) =
 *   let rec constraint_to_type = function
 *     | Type (name, cons) ->
 *        let cons2 = List.map constraint_to_type cons in
 *        let cons_v = List.fold_left (fun a -> function | None -> a | Some(n) -> n :: a) [] cons2 in
 *        (match List.length cons_v with
 *         | 0 -> Some(Ast.Type name)
 *         | n when n <> List.length cons2 -> None
 *         | n -> Some(Parameterized (name, cons_v)))
 *     | _ -> None in
 *
 *   let rec applyType cons_type node =
 *     match node with
 *      | Func func as f ->
 *         (\* Printf.printf "func name %s returns %s\n" func.name (type_to_string cons_type); *\)
 *         (match cons_type with
 *          | Parameterized ("Func", cons_params) ->
 *             (match cons_params with
 *              | [] -> failwith "???"
 *              | ret :: ptypes ->
 *                 let newparams =
 *                   List.map2
 *                     applyType
 *                     ptypes
 *                     (List.rev func.params |> List.map (fun v -> Def v)) in
 *                 func.ret_type <- ret;
 *                 ignore newparams;
 *                 f)
 *         | _ -> f)
 *      | Def v as d ->
 *         (\* Printf.printf "def (%s) is typed %s\n" v.name (type_to_string cons_type); *\)
 *         v.defType <- Some cons_type;
 *         d
 *      | Let (v, value) ->
 *         (\* Printf.printf "let (%s) is typed %s\n" v.name (type_to_string cons_type); *\)
 *         v.varType <- Some cons_type;
 *         Let(v, value)
 *      | Return v ->
 *         Printf.printf "return is typed %s\n" (type_to_string cons_type);
 *         v.coraltype <- Some(cons_type);
 *         Return v
 *      | n -> n
 *   in
 *   let apply = function
 *     | {term={node=Some(e)}; cons=(Type _ as t)} ->
 *        (match constraint_to_type t with
 *         | None -> ()
 *         | Some(cons_type) -> ignore (applyType cons_type e))
 *     | _ -> () in
 *
 *   solution.Solver.dependent_terms
 *   |> TermMap.bindings
 *   |> List.map (fun (t, edges) -> EdgeSet.elements edges)
 *   |> List.concat
 *   |> List.iter (fun e -> apply e)
 *
 * let run m =
 *   let graph = Graph.create () in
 *   let term = createGraph graph m in
 *   ignore term;
 *   let solution = Solver.init graph in
 *   let solution = Solver.solve solution in
 *   applySolution solution;
 *   Ast.show m; flush stdout;
 *   m *)
