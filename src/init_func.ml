(* An AST rewriter that converts module-level statements into an ".init" function *)

open Ast
type moduleExprAction = Noop | AddToMain | IsMain

let moduleExprAction = function
  | Module _ -> Noop
  | Func {name=name; ret_type=rettype} when name == ".init" -> IsMain
  | Func _ -> Noop
  | _ -> AddToMain

let run = function
  | Module {name=name;lines=lines} ->
     let rec loop_line main leave_lines move_lines lines =
       match lines with
       | [] ->
          let main = match main with
            | None ->
               let body = Ast.Block (
                              List.rev (
                                  Return {node=Tuple [];coraltype=None} :: move_lines)) in
               Ast.Func (newFunc(".init", Type("Void"), [], body))
            | Some f -> f
          in Module {name=name; lines=List.rev @@ main::leave_lines}
       | line :: xs ->
          match moduleExprAction line with
          | Noop -> loop_line main (line::leave_lines) move_lines xs
          | IsMain -> loop_line (Some line) (line::leave_lines) move_lines xs
          | AddToMain -> loop_line main leave_lines (line::move_lines) xs
     in loop_line None [] [] lines
  | _ -> failwith "InitFuncBuilder: run on modules only"
