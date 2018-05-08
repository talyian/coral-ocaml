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

let rec constraint_to_type = function
  | Graph.Type(s, []) -> Ast.Type s
  | Graph.Type(s, params) -> Ast.Parameterized(s, List.map constraint_to_type params)
  | c -> failwith ("unhandled type" ^ Graph.cons_to_string c)

let rec createGraph g node =
  match node with
  | Empty -> Graph.addTerm g "empty" Empty
  | Module modinfo as m ->
     let fold (terms, g) a =
       let (t, g) = createGraph g a in
       (t::terms, g) in
     let terms, gg = List.fold_left fold ([], g) modinfo.lines in
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
         let gg = Graph.constrain gg blockterm (Graph.Term (List.hd terms).name) in
         blockterm, gg)
  | Multifunc (name, fdata_list) as mfunc ->
     let terms, graph =
       let folder (tt, gg) f = let fterm, gg = createGraph gg (Func f) in fterm :: tt, gg in
       List.fold_left folder ([], g) fdata_list in
     let mfunc_term, graph = Graph.addTerm graph ("mfn." ^ name) mfunc in
     let graph =
       let terms = List.rev terms in
       let options = List.map (fun (t:Graph.term) -> Graph.Term t.name) terms in
       let mfunc_type = (Graph.OneOf options ) in
       Graph.constrain graph mfunc_term mfunc_type in
     mfunc_term, graph
  | Func fdata as func ->
     let fold (terms, gg1) param =
       let (t, gg2) = createGraph gg1 param in
       (t::terms, gg2) in
     let terms, gg = List.fold_left fold ([], g) fdata.params in
     let terms = List.map (fun (x:Graph.term) -> Graph.Term x.name) @@ List.rev terms in
     let fterm, gg = Graph.addTerm gg ("fn." ^ fdata.name) func in
     let bodyTerm, gg = createGraph gg fdata.body in
     let retTerm, gg = Graph.addTerm gg (fdata.name ^ ".ret") Empty in
     let gg =
       match fdata.ret_type with
       | Ast.Type "" -> gg
       | n -> Graph.constrain gg retTerm (type_to_constraint fdata.ret_type) in
     let gg =
       match fdata.body with
       | Empty -> gg
       | body -> Graph.constrain gg retTerm (Graph.Term bodyTerm.name) in
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
     let gg = Graph.constrain gg condterm (Graph.Type ("Int1", [])) in
     let ifbodyterm, gg = createGraph gg ifbody in
     let elsebodyterm, gg = createGraph gg elsebody in
     let gg = Graph.constrain gg ifterm
                (Graph.OneOf [Graph.Term ifbodyterm.name; Graph.Term elsebodyterm.name]) in
     ifterm, gg
  | Return v as ret ->
     let val_term, gg = createGraph g v.node in
     let term, gg = Graph.addTerm gg ("ret." ^ val_term.name) ret in
     let gg = Graph.constrain gg term (Graph.Term val_term.name) in
     term, gg
  | IntLiteral i as inode ->
     let term, gg = Graph.addTerm g ("i" ^ i) inode in
     let gg = Graph.constrain gg term (Graph.Type ("Int64", [])) in
     term, gg
  | FloatLiteral i as inode ->
     let term, gg = Graph.addTerm g ("f" ^ i) inode in
     let gg = Graph.constrain gg term (Graph.Type ("Float64", [])) in
     term, gg
  | Binop (op, lhs, rhs) as binop ->
     let term, gg = Graph.addTerm g ("op." ^ op) binop in
     let lterm, gg = createGraph gg lhs in
     let rterm, gg = createGraph gg rhs in
     let gg =
       let callee = Graph.Term op in
       let args = [Graph.Term lterm.name; Graph.Term rterm.name] in
       let overload_idx, gg = Graph.addTerm gg "overload" binop in
       Graph.constrain gg term (Graph.Call (callee, args, Graph.Term (overload_idx.name))) in
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
  | Call {callee=callee;args=args} as call->
     let calleeterm, gg = createGraph g callee in
     let fold (terms, g) a =
       let (t, g) = createGraph g a in
       (t::terms, g) in
     let argterms, gg = List.fold_left fold ([], gg) @@ List.rev args in
     let term, gg = Graph.addTerm gg ("call." ^ calleeterm.name) call in
     let gg =
       let c1 = Graph.Term calleeterm.name in
       let c2 = List.map (fun (t:Graph.term) -> Graph.Term t.name) argterms in
       let overload_idx, gg = Graph.addTerm gg ("overload." ^ calleeterm.name) call in
       Graph.constrain gg term (Graph.Call (c1, c2, Graph.Term overload_idx.name)) in
     term, gg
  | StringLiteral s as snode ->
     let term, gg = Graph.addTerm g ("s" ^ Ast.string_name_escape s) snode in
     let gg = Graph.constrain gg term (Graph.Type ("String", [])) in
     term, gg
  | TupleDef info ->
     let fold_field (terms, gg) (fname, ftype) =
       let field_term, graph =
         let name = "(MemberType) " ^ info.name ^ "::" ^ fname in
         Graph.addTerm gg name Empty in
       let index_term, graph =
         let name = "(MemberIndex) " ^ info.name ^ "::" ^ fname in
         Graph.addTerm graph name Empty in
       let index = string_of_int @@ List.length terms in
       let graph = Graph.constrain graph field_term (type_to_constraint ftype) in
       let graph = Graph.constrain graph index_term (Graph.Type (index, [])) in
       field_term :: terms, graph in
     let fields, graph = List.fold_left fold_field ([], g) info.fields in
     let tuple_term, graph = Graph.addTerm graph info.name (TupleDef info) in
     let graph =
       let field_types = List.map (fun (t:Graph.term) -> Graph.Term t.name) fields in
       let ret_type = Graph.Type (info.name, []) in
       let constructor_type = Graph.Type("Func", field_types @ [ret_type]) in
       Graph.constrain graph tuple_term constructor_type in
     tuple_term, graph
  | Let (var, rhs) as letexpr ->
     let value, gg = createGraph g rhs in
     let term, gg = Graph.addTerm gg var.name letexpr in
     let gg = Graph.constrain gg term (Term value.name) in
     term, gg
  | Member mem as member ->
     let baseterm, gg = createGraph g mem.base in
     let term, gg = Graph.addTerm gg (baseterm.name ^ "::" ^ mem.memberName) member in
     let iterm, gg = Graph.addTerm gg (term.name ^ ".idx") member in
     let gg =
       let base = Graph.Term baseterm.name in
       let cons = Graph.Member (base, mem.memberName, Graph.Term iterm.name) in
       Graph.constrain gg term cons in
     term, gg
  | x ->
     Printf.printf "createGraph: %s\n" (nodeName node);
     Graph.addTerm g (nodeName x) x

let applySolution gg m =
  Graph.TermMap.iter (fun term cons ->
      match term.value with
      | Func info ->
         (* let show () = Printf.printf "%s (%s) --> %s\n"
          *         (Ansicolor.as_color (Bold RED) term.name)
          *         (Ast.nodeName term.value)
          *         (Graph.cons_to_string cons) in *)
         let show () = () in
         (try
            match constraint_to_type cons with
            | Ast.Parameterized ("Func", params) -> info.ret_type <- List.hd @@ List.rev params
            | _ -> show ()
          with | e -> show ())
      | Def p -> p.defType <- Some(constraint_to_type cons);
      | Return p -> p.coraltype <- Some(constraint_to_type cons);
      | Let (var, expr) -> var.varType <- Some(constraint_to_type cons)
      | Member mem ->
         (match cons with
         | Graph.Type("Index", [Graph.Type(n, [])]) ->
            mem.memberIndex <- int_of_string n
         | _ -> (* don't need to set type on member *)())
      (* ignore known type nodes *)
      | TupleDef _
      | Block _
      | FloatLiteral _ | IntLiteral _ | Empty _ -> ()
      | StringLiteral _ -> ()
        (* TODO: if we tag binops with type
         we can resolve overloaded operators *)
      | Binop _ -> ()
      | If _ -> ()
      | Tuple _ -> () (* TODO: we need to populate these *)
      | Call {callee=Var ({target=Some(Multifunc (mf_name, mf_funcs))} as cinfo)} ->
         (match cons with
          | Graph.Type ("Overload", [Graph.Type (index_str, [])]) ->
             let idx = int_of_string index_str in
             cinfo.target <- Some(Func (List.nth mf_funcs idx))
          | _ -> ());
         if Graph.config.debug then
           Printf.printf "%s (%s) --> %s\n"
             (Ansicolor.as_color (Bold YELLOW) term.name)
             (Ast.nodeName term.value) (Graph.cons_to_string cons);
      | _ ->
         if Graph.config.debug then
           Printf.printf "%s (%s) --> %s\n" (Ansicolor.as_color (Bold CYAN) term.name)
             (Ast.nodeName term.value) (Graph.cons_to_string cons);
         ()
  ) gg.Graph.constraints

let run m =
  let gg = Graph.empty in

  (* Initialize builtin known operand types *)
  let arith_op_type = (Graph.Type("Func", [Graph.Free 0;Graph.Free 0;Graph.Free 0])) in
  let bool_op_type = (Graph.Type("Func", [Graph.Free 0;Graph.Free 0;Graph.Type("Int1", [])])) in
  let fold_op_is optype gg o =
    let op, gg = Graph.addTerm gg o Empty in
    Graph.constrain gg op optype in
  let gg = List.fold_left (fold_op_is arith_op_type) gg ["+"; "*"; "-"; "/"; "%"] in
  let gg = List.fold_left (fold_op_is bool_op_type) gg ["="; "<";">"; ">="; "<="; "!="] in

  (* Ast.show m; *)
  let term, graph = createGraph gg m in
  if Graph.config.debug then Graph.show graph;
  let solution = Graph.solve graph in
  if Graph.config.debug then Graph.show solution;
  applySolution solution m;
  flush stdout;
  m
