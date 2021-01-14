open Coral_core
open Base

(** To import a module, [import Foo.Bar] will find the file "foo/bar.coral" relative to a system
    import-root setting.

    ** Paths: *** Absolute imports **** System imports **** Project dependency imports *** Relative
    imports

    ** which imports *)

module Imports = struct
  type node = Ast_node.Adt.node
  type resolved = {full_path: string; expr: node}

  let new_resolved full_path expr = {full_path; expr}

  type t =
    { main: Ast_node.Adt.node
    ; imports: (Ast_node.Adt.node, Ast_node.Adt.node, Ast_node.Adt.Node.comparator_witness) Map.t
    }

  let add t key data = {t with imports= Map.set ~key ~data t.imports}
end

let try_read_file path = try Some (Stdio.In_channel.read_all path) with _ -> None

(* We're parameterized over the parser function type so that this module can be compiled
   independently from the frontend *)
type parse_function_t =
  string -> (Ast_node.Adt.node, Coral_frontend.Frontend.parseError) Base.Result.t

let resolve ~(parse_func : parse_function_t) ~(src : string) =
  (* load_import : string -> resolved option *)
  let load_import ~path : Imports.resolved option =
    (* Given a "relative" import path, resolve the path and load the source from that path *)
    let filename = String.concat ~sep:"/" path ^ ".coral" in
    Base.List.find_map
      ~f:(fun parent_path ->
        let open Option.Let_syntax in
        let full_path = parent_path ^ filename in
        let%bind text = try_read_file full_path in
        let%bind expr = parse_func text |> Result.ok in
        Option.some @@ Imports.new_resolved full_path expr)
      ["../../../examples"; "../../../examples/stdlib"] in
  let%bind.Result expr = parse_func src in
  let init = {Imports.main= expr; Imports.imports= Map.empty (module Ast_node.Adt.Node)} in
  let f (imports : Imports.t) _node : Imports.t = imports in
  let _imports = Ast_node.Adt.fold_info ~init ~f expr in
  Ok _imports
