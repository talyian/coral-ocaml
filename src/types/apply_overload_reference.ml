(* After Name and type resolution has run, we should have overload indices for any var that refers
   to an overload-set. So we should be able to replace the mapped value to the direct overload item
   in the set. *)
open! Base
open! Coral_core

(* let fix_name_resolution (names : Names.t) (types : Resolver.Resolver.t) =
 *   (\* Given a var and a reference, if that reference is an overload change it to the overload slot *\)
 *   let refs =
 *     Map.fold types.overload_index ~init:names.refs ~f:(fun ~key ~data:i refs ->
 *         (\* Stdio.printf "Writing down overload %d for %s\n" i (Ast.show_node key); *\)
 *         match fst key with
 *         | Ast.Binop { callee; _ } | Ast.Call { callee; _ } -> (
 *             match Names.deref names callee with
 *             | Some (Ast.Overload { items; name = _ }, _) ->
 *                 Map.set ~key:callee ~data:(List.nth_exn items i) refs
 *             | _ -> failwith "bad overload target" )
 *         | _ -> refs)
 *   in
 *   { names with refs } *)
