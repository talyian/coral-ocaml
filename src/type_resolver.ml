open Ast

type term = { name: string }
type cons =
  | Term of term
  | Type of coraltype
  | Call of cons * cons list
  | Member of cons * string

type graph = {
    mutable active_terms: term list;
    mutable inactive_terms: term list;
    mutable terms: term list;
    mutable edges: (term * cons) list; }

let rec createGraph g = function
  | Module list -> List.fold_left (fun _g i -> createGraph _g i) g list
  (* | Func(name, ret, params, body) ->
   *    run1 body
   * | If (cond, ifbody, elsebody) -> run1 cond; run1 ifbody; run1 elsebody
   * | Call (callee, args) -> run1 callee; List.iter run1 args;
   *)
  | Func(name, ret, params, body) as f ->
     let g = List.fold_left (fun g (p:defNode) -> (p.name, Def p) :: g) g params in
     let g = (name, f) :: g in
     createGraph g body
  | If (cond, i, e) -> createGraph (createGraph (createGraph g cond) i) e
  | Call (callee, args) ->
     let g = createGraph g callee in
     let g = List.fold_left createGraph g args in
     (* TODO TODO *)
     g
  | IntLiteral i as v -> ("i", v) :: g
  | StringLiteral i as v -> ("s", v) :: g
  | FloatLiteral i as v -> ("f", v) :: g
  | Var i as v -> (i.name, v) :: g
  | Return v -> createGraph g v
  | Block list -> List.fold_left createGraph g list
  | Binop (op, lhs, rhs) -> createGraph (createGraph g lhs) rhs
  | Empty -> g
  | n -> Printf.printf "Resolving Type: %s\n" (nodeName n); g


let run m =
  let graph = createGraph [] m in m
