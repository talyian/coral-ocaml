(* An AST rewriter inserts "returns" in the last line of a function *)

open Ast

let rec find_function = function
  | Module lines -> Module(List.map find_function lines)
  | Func {name=name; ret_type=ret_type; params=params; body=Empty} ->
     Func (newFunc(name, ret_type, params, Empty))
  | Func {name=name; ret_type=ret_type; params=params; body=body} ->
     Func (newFunc(name, ret_type, params, rewrite ret_type body))
  | n -> n
and rewrite retType = function
  | If (cond, ifbody, elsebody) ->
     If (cond,
         rewrite retType ifbody,
         rewrite retType elsebody)
  | Block lines ->
     let rec loop x asdf =
       match List.rev asdf with
       | Empty _ as e:: xs -> loop (e::x) xs
       | Comment _ as e :: xs -> loop (e::x) xs
       | e :: xs -> rewrite retType e :: xs
       | [] -> [Return {node=Empty;coraltype=None}]
     in Block ((loop [] lines) |> List.rev)
  | Return _ as r -> r
  | Empty -> Return {node=Tuple [];coraltype=None}
  | _ as e ->
     match retType with
     | Type("Void") -> Block [e; Return {node=Tuple [];coraltype=None}]
     | _ -> Return {node=e;coraltype=None}

let run = find_function
