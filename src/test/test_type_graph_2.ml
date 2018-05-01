open Type_graph_2

module Test_Type_Graph = struct

type node =
  | ENode
  | SNode of string
  | INode of int

module Graph = GraphF(struct
   type t = node
   let show = function
     | ENode -> "(null)"
     | SNode s -> s
     | INode i -> string_of_int i
   let empty = ENode
   let cmp = compare
end)

let test_assignment () =
  Printf.printf "Test Assignment\n";
  (* The existing type environment *)
  let env = Graph.empty in
  let a0, env = Graph.addTerm env "a" ENode in
  let env = Graph.constrain env a0 (Graph.Type ("int6", [])) in

  Printf.printf "Case 1: b = a; c = b\n";
  let gg = Graph.childOf env in
  let a0, gg = Graph.addTerm gg "a" ENode in
  let t0, gg = Graph.addTerm gg "b" ENode in
  let t1, gg = Graph.addTerm gg "c" ENode in
  let gg = Graph.constrain gg a0 (Graph.Type ("int6", [])) in
  let gg = Graph.constrain gg t0 (Graph.Term a0.name) in
  let gg = Graph.constrain gg t1 (Graph.Term t0.name) in
  Graph.showColor (5, 3, 2) gg;
  let ss = Graph.solve gg in
  Graph.show ss;

  Printf.printf "Case 2: b = a; b = c\n";
  let gg = Graph.childOf env in
  let a0, gg = Graph.addTerm gg "a" ENode in
  let t0, gg = Graph.addTerm gg "b" ENode in
  let t1, gg = Graph.addTerm gg "c" ENode in
  let gg = Graph.constrain gg a0 (Graph.Type ("int6", [])) in
  let gg = Graph.constrain gg t0 (Graph.Term a0.name) in
  let gg = Graph.constrain gg t0 (Graph.Term t1.name) in
  Graph.showColor (5, 3, 2) gg;
  let ss = Graph.solve gg in
  Graph.show ss

let test_funcalls () =
  Printf.printf "Case Call 1: return type inference\n";
  let gg = Graph.empty in
  let fn, gg = Graph.addTerm gg "foo" ENode in
  let gg = Graph.constrain gg fn
             (Graph.Type ("Func", [Graph.Type ("Int4", []);
                                   Graph.Type ("Int5", []);
                                   Graph.Type ("Int6", [])])) in
  let a, gg = Graph.addTerm gg "a" ENode in
  let b, gg = Graph.addTerm gg "b" ENode in
  let fc, gg = Graph.addTerm gg "foo.call" ENode in
  let gg = Graph.constrain gg fc (Graph.Call (Term fn.name, [Term a.name; Term b.name])) in
  Graph.showColor (5, 3, 2) gg;
  Graph.solve gg |> Graph.show

let test_funcalls_generic () =
  Printf.printf "Case Call 1: parametricly polymorphic\n";
  let gg = Graph.empty in
  let fn, gg = Graph.addTerm gg "foo" ENode in
  let gg = Graph.constrain gg fn
             (Graph.Type ("Func", [Graph.Free 0;
                                   Graph.Free 0;
                                   Graph.Free 0])) in
  let a, gg = Graph.addTerm gg "a" ENode in
  let b, gg = Graph.addTerm gg "b" ENode in
  let c, gg = Graph.addTerm gg "c" ENode in
  let fc, gg = Graph.addTerm gg "foo.call" ENode in
  let gg = Graph.constrain gg fc (Graph.Call (Term fn.name, [Term a.name; Term b.name])) in

  let fc, gg = Graph.addTerm gg "foo.call" ENode in
  let gg = Graph.constrain gg fc (Graph.Call (Term fn.name, [Type ("Ptr", []); Term c.name])) in
  let gg = Graph.constrain gg a (Graph.Type ("Int2", [])) in
  Graph.showColor (5, 3, 2) gg;
  Graph.solve gg |> Graph.show;

  Printf.printf "Case Call 2: adhoc polymorphic\n";
  let gg = Graph.empty in
  let fn, gg = Graph.addTerm gg "foo" ENode in

  let gg =
    let i = Graph.Type ("Int2", []) in
    let f = Graph.Type ("Float4", []) in
    let p = Graph.Type ("Ptr", []) in
    let t = Graph.OneOf [
                Graph.Type ("Func", [i; i]);
                Graph.Type ("Func", [f; f]);
                Graph.Type ("Func", [p; p])] in
    Graph.constrain gg fn t in
  let a, gg = Graph.addTerm gg "a" ENode in
  let gg = Graph.constrain gg a (Graph.Type("Float4", [])) in
  let call, gg = Graph.addTerm gg "res" ENode in
  let gg = Graph.constrain gg call (Graph.Call (Term fn.name, [Term a.name])) in

  let a, gg = Graph.addTerm gg "a" ENode in
  let gg = Graph.constrain gg a (Graph.Type("Int2", [])) in
  let call, gg = Graph.addTerm gg "res" ENode in
  let gg = Graph.constrain gg call (Graph.Call (Term fn.name, [Term a.name])) in
  Graph.showColor (5, 3, 2) gg;
  Graph.show @@ Graph.solve gg;

  Printf.printf "Recursive function: factorial\n";
  let gg = Graph.empty in
  let fn, gg = Graph.addTerm gg "foo" ENode in
  let ifblock, gg = Graph.addTerm gg "if" ENode in
  let thenblock, gg = Graph.addTerm gg "then" ENode in
  let elseblock, gg = Graph.addTerm gg "else" ENode in
  let gg = Graph.constrain gg thenblock (Graph.Type ("Int10", [])) in
  let gg = Graph.constrain gg elseblock (Graph.Term fn.name) in
  let gg = Graph.constrain gg ifblock (Graph.OneOf [Graph.Term thenblock.name;
                                                    Graph.Term elseblock.name]) in
  let gg = Graph.constrain gg fn (Graph.Term ifblock.name) in
  Graph.showColor (5, 3, 2) gg;
  Graph.show @@ Graph.solve gg;

  Printf.printf "Recursive function: factorial #2\n";
  let gg = Graph.empty in
  let fn, gg = Graph.addTerm gg "factorial" ENode in
  let ret, gg = Graph.addTerm gg "factorial.ret" ENode in
  let n, gg = Graph.addTerm gg "n" ENode in
  let ifblock, gg = Graph.addTerm gg "if" ENode in
  let thenblock, gg = Graph.addTerm gg "then" ENode in
  let elseblock, gg = Graph.addTerm gg "else" ENode in

  let gg = Graph.constrain gg thenblock (Graph.Type ("Int1", [])) in
  let gg = Graph.constrain gg elseblock (Graph.Call (Graph.Term fn.name, [Graph.Term n.name]))  in
  let gg = Graph.constrain gg ifblock (Graph.OneOf [Graph.Term thenblock.name;
                                                    Graph.Term elseblock.name]) in
  let gg = Graph.constrain gg ret (Graph.Term ifblock.name) in
  let gg = Graph.constrain gg fn (Graph.Type ("Func", [
                                        Graph.Term n.name;
                                        Graph.Term ret.name])) in
  Graph.showColor (5, 3, 2) gg;
  Graph.show @@ Graph.solve gg;
  ()
let () =
  test_assignment ();
  test_funcalls();
  test_funcalls_generic()
end
