open Coral_core
open Base

(** To import a module, [import Foo.Bar] will find the file "foo/bar.coral" relative to a system
    import-root setting.

    ** Paths: *** Absolute imports **** System imports **** Project dependency imports *** Relative
    imports

    ** which imports *)

type node = Ast.node

module NodeM = struct
  module NodeM = struct type t = Ast.node [@@deriving compare, sexp_of] end
  include NodeM
  include Comparable.Make (NodeM)
end

module Imports = struct
  type resolved = {full_path: string; expr: node}

  let new_resolved full_path expr = {full_path; expr}

  type t = {main: node; imports: node Map.M(NodeM).t}

  let add t key data = {t with imports= Map.set ~key ~data t.imports}
  let get key {main; imports} = Map.find_exn imports key
end

let try_read_file path = try Some (Stdio.In_channel.read_all path) with _ -> None

(* We're parameterized over the parser function type so that this module can be compiled
   independently from the frontend *)
(* TODO Now that this is in frontend, this seems unnecessary *)
type parse_function_t = string -> (node, Frontend.parseError) Base.Result.t

let resolve ~(parse_func : parse_function_t) ~(src : string) =
  (* load_import : string -> resolved Or_error.t *)
  let load_import ~path : (Imports.resolved, Error.t) Result.t =
    (* Given a "relative" import path, resolve the path and load the source from that path *)
    let filename = String.concat ~sep:"/" path ^ ".coral" in
    let rec find_result = function
      | [] -> Or_error.error_string ("file not found: " ^ filename)
      | path :: rest ->
          let full_path = path ^ "/" ^ filename in
          if Caml.Sys.file_exists full_path then
            Stdio.In_channel.read_all full_path
            |> parse_func
            |> Result.map_error ~f:(fun e -> Error.of_string @@ Frontend.show_parseError e)
            |> Result.map ~f:(fun expr -> {Imports.full_path; expr})
          else find_result rest in
    find_result ["../../../examples/"; "../../../examples/stdlib/"] in
  (* To run import resolution, we start from the main file and run load_import recursively against
     all its imports *)
  let%bind.Result expr = parse_func src in
  let init = {Imports.main= expr; Imports.imports= Map.empty (module NodeM)} in
  let f (imports : Imports.t) _node : Imports.t =
    match !_node with
    | Ast.Import {path; names; _} -> (
      match load_import ~path with
      | Error e -> failwith @@ Error.to_string_hum e
      | Ok resolved -> Imports.add imports _node resolved.expr )
    | _ -> imports in
  Ok (Ast.fold_info ~init ~f expr)
