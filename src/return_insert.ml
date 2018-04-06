(* An AST rewriter inserts "returns" in the last line of a function *)

open Ast

let rec find_function = function
  | Module lines -> Module(List.map find_function lines)
  | Func (a, b, c, Empty) -> Func(a, b, c, Empty)
  | Func (a, b, c, d) -> Func(a, b, c, rewrite b d)
  | n -> n
and rewrite retType = function
  | If (cond, ifbody, elsebody) -> If (cond, rewrite retType ifbody, rewrite retType elsebody)
  | Block lines ->
     let rec loop x asdf =
       match List.rev asdf with
       | Empty _ as e:: xs -> loop (e::x) xs
       | Comment _ as e :: xs -> loop (e::x) xs
       | e :: xs -> rewrite retType e :: xs
       | [] -> [Return Empty]
     in Block ((loop [] lines) |> List.rev)
  | Return _ as r -> r
  | _ as e ->
     match retType with
     | Type("Void") -> Block [e; Return Empty]
     | _ -> Return e

let run = find_function
