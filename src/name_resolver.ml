open Ast

module StringMap = Map.Make(String)

type scope = {
    parent: scope option;
    names: Ast.node StringMap.t;
}
let rec findName name scope =
  try
    Some(StringMap.find name scope.names)
  with Not_found ->
    match scope.parent with
    | Some (p) -> findName name p
    | None -> None

let addName name value scope = {scope with names=StringMap.add name value scope.names}

(* point all the Var nodes to a preceding expression *)
let rec run1 scope = function
  | Module lines as m ->
     let scope = List.fold_left (fun s p -> fst (run1 s p)) scope lines in
     scope, m
  | Func(name, ret_type, params, body) as f ->
     let scope = addName name f scope in
     let scope = List.fold_left (fun s p -> fst(run1 s (Def p))) scope params in
     let scope, body = run1 scope body in
     scope, f
  | If(cond, ifbody, elsebody) as x ->
     ignore (run1 scope cond);
     ignore (run1 scope ifbody);
     ignore (run1 scope elsebody);
     scope, x
  | Binop(op, lhs, rhs) as x ->
     ignore (run1 scope lhs);
     ignore (run1 scope rhs);
     scope, x
  | IntLiteral i as x -> scope, x
  | FloatLiteral i as x -> scope, x
  | StringLiteral s as x -> scope, x
  | Comment c as x -> scope, x
  | Var v as x ->
     (match findName v.name scope with
      | Some(e) -> v.target <- Some(e)
      | None -> Printf.printf "\027[1;31mmissing reference: %s\027[0m\n" v.name
     ); scope, x
  | Def v as x -> addName v.name x scope, x
  | Call (callee, args) as x ->
     ignore (run1 scope callee);
     let rec loop = function | [] -> () | x :: xs -> ignore (run1 scope x); loop xs
     in loop args;
     scope, x
  | Block lines as x ->
     let final_scope = List.fold_left (fun s p -> fst (run1 s p)) scope lines in
     ignore final_scope;
     scope, x
  | Tuple t as x ->
     let scope = List.fold_left (fun s p -> fst (run1 s p)) scope t in
     scope, x
  | Return v as x-> ignore (run1 scope v); scope, x
  | Empty as x -> scope, x
  | x ->
     Printf.printf "Warning: Unhandled node for name resolver\n";
     Ast.show x;
     scope, x
let run x = snd (run1 {parent=None; names=StringMap.empty} x)
