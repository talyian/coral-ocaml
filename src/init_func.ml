(** An AST rewriter that converts module-level statements into an ".init" function.

    We could run this before import/name as a pretty naive pass, but using type information we can
    also precisely pinpoint which module lines are constexpr globals and which lines need to be
    mutable globals initialized in .init.

    For now, we can do it the naive way and only support simple literals, and initialize everything
    else at the begining of .init. *)

open Coral_core

(* type expr_class = Global of Ast_node.Adt.node | Init of Ast_node.Adt.node
 *
 * let classify = function
 *   | Ast_node.Adt.Node.Extern _ as e -> Global e
 *   | Ast_node.Adt.Node.Let _ as e -> Global e
 *   | Ast_node.Adt.Node.Func _ as e -> Global e
 *   | e -> Init e
 *
 * let run e =
 *   match e with
 *   | Ast_node.Adt.Node.Module {name; lines; _} ->
 *       let leave_lines, move_lines =
 *         let rec loop_line leave_lines move_lines = function
 *           | line :: xs -> (
 *             match classify line with
 *             | Global line -> loop_line (line :: leave_lines) move_lines xs
 *             | Init line -> loop_line leave_lines (line :: move_lines) xs )
 *           | [] -> (leave_lines, move_lines) in
 *         loop_line [] [] lines in
 *       let main =
 *         let body =
 *           Ast_node.Make.returnNode (Ast_node.Make.tuple []) :: move_lines
 *           |> List.rev |> Ast_node.Make.block in
 *         let ret_type = Some (Ast_node.Type.Name "Void") in
 *         Ast_node.Make.funcNode ".init" ret_type [] body in
 *       Ast_node.Make.moduleNode name (List.rev (main :: leave_lines))
 *   | _ -> failwith "InitFuncBuilder: run on modules only" *)
