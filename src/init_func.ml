(** An AST rewriter that converts module-level statements into an ".init"
   function.

   We could run this before import/name as a pretty naive pass, but using type
   information we can also precisely pinpoint which module lines are constexpr
   globals and which lines need to be mutable globals initialized in .init.

   For now, we can do it the naive way and only support simple constexprs *)

open Coral_core

type expr_class = Global of Ast.node | Init of Ast.node

let classify = function
  | (Ast.Extern _, _) as e -> Global e
  | (Ast.Let (_var, _value), _) as e -> Global e
  | e -> Init e

let run e =
  match e with
  | Ast.Module { name; lines; _ }, _ ->
      let leave_lines, move_lines =
        let rec loop_line leave_lines move_lines = function
          | line :: xs -> (
              match classify line with
              | Global line -> loop_line (line :: leave_lines) move_lines xs
              | Init line -> loop_line leave_lines (line :: move_lines) xs )
          | [] -> (leave_lines, move_lines)
        in
        loop_line [] [] lines
      in
      let main =
        let body =
          Ast.mm
          @@ Ast.Block
               (List.rev
                  ((Ast.mm @@ Ast.Return (Ast.mm @@ Ast.Tuple [])) :: move_lines))
        in
        let ret_type = Some (Type.Name "Void") in
        Ast.Make.funcNode (".init", ret_type, [], body)
      in
      Ast.mm @@ Ast.Module { name; lines = List.rev @@ (main :: leave_lines) }
  | _ -> failwith "InitFuncBuilder: run on modules only"
