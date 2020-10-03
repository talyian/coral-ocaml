open Ast
module StringMap = Map.Make (String)

type scope = { parent : scope option; names : Ast.node StringMap.t }

let rec findName name scope =
  try Some (StringMap.find name scope.names)
  with Not_found -> (
    match scope.parent with Some p -> findName name p | None -> None )

let addName name value scope =
  { scope with names = StringMap.add name value scope.names }

(* point all the Var nodes to a preceding expression *)
let rec run1 scope = function
  | Module { lines; _ } as m ->
      let scope = List.fold_left (fun s p -> fst (run1 s p)) scope lines in
      (scope, m)
  | Func { name; ret_type = _ret_type; params; body } as f ->
      let scope = addName name f scope in
      let scope = List.fold_left (fun s p -> fst (run1 s p)) scope params in
      let scope, _body = run1 scope body in
      (scope, f)
  | Multifunc { name; func; next = _next } as mf ->
      let mf_scope = addName name mf scope in
      ignore @@ run1 scope !func;
      (* List.map (fun f -> run1 mf_scope !f) funcs |> ignore; *)
      (mf_scope, mf)
  | If (cond, ifbody, elsebody) as x ->
      ignore (run1 scope cond);
      ignore (run1 scope ifbody);
      ignore (run1 scope elsebody);
      (scope, x)
  | Binop { name = _op; args = [ lhs; rhs ]; _ } as x ->
      ignore (run1 scope lhs);
      ignore (run1 scope rhs);
      (scope, x)
  | IntLiteral _ as x -> (scope, x)
  | FloatLiteral _ as x -> (scope, x)
  | StringLiteral _ as x -> (scope, x)
  | Comment _ as x -> (scope, x)
  | Var v as x ->
      let _ =
        match findName v.name scope with
        | Some e -> v.target <- Some e
        | None ->
            (* Printf.printf "missing reference: %s\n" v.name; *)
            ()
      in
      (scope, x)
  | Let (v, expr) as x -> (addName v.name x (fst (run1 scope expr)), x)
  | Set (_v, expr) as x -> (fst (run1 scope expr), x)
  | Def v as x -> (addName v.name x scope, x)
  | Call { callee; args; _ } as x ->
      ignore (run1 scope callee);
      let rec loop = function
        | [] -> ()
        | x :: xs ->
            ignore (run1 scope x);
            loop xs
      in
      loop args;
      (scope, x)
  | Block lines as x ->
      let final_scope =
        List.fold_left (fun s p -> fst (run1 s p)) scope lines
      in
      ignore final_scope;
      (scope, x)
  | Tuple t as x ->
      let scope = List.fold_left (fun s p -> fst (run1 s p)) scope t in
      (scope, x)
  | TupleDef def as tuple ->
      let scope = addName def.name tuple scope in
      (scope, tuple)
  | Member mem as member ->
      let scope, _ = run1 scope mem.base in
      (scope, member)
  | Return { node = v; _ } as x ->
      ignore (run1 scope v);
      (scope, x)
  | Empty as x -> (scope, x)
  | _ -> failwith "unhandled"

let run x = snd (run1 { parent = None; names = StringMap.empty } x)
