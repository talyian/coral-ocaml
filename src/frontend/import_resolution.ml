open Coral_core
open Base

(** To import a module, [import Foo.Bar] will find the file "foo/bar.coral" relative to a system
    import-root setting.

    ** Paths: *** Absolute imports **** System imports **** Project dependency imports *** Relative
    imports

    ** which imports *)

module Imports = struct
  type resolved = {full_path: string; expr: Ast.node}
  type t = {main: Ast.node; imports: Ast.node Map.M(Ast).t}

  let add t key data = {t with imports= Map.set ~key ~data t.imports}
  let get key {imports; _} = Map.find_exn imports key
end

let try_read_file path = try Some (Stdio.In_channel.read_all path) with _ -> None

(* We're parameterized over the parser function type so that this module can be compiled
   independently from the frontend *)
(* TODO Now that this is in frontend, this seems unnecessary *)
type parse_function_t = string -> (Ast.node, Frontend.parseError) Base.Result.t

let resolve ~(parse_func : parse_function_t) ~(src : string) =
  (* load_import : string -> resolved Or_error.t *)
  let load_import ~path : (Imports.resolved, Error.t) Result.t =
    (* Given a "relative" import path, resolve the path and load the source from that path *)
    let filename = String.concat ~sep:"/" path ^ ".coral" in
    let rec find_result = function
      | [] -> Or_error.error_string ("file not found: " ^ filename)
      | found_path :: rest ->
          let full_path = found_path ^ "/" ^ filename in
          if Caml.Sys.file_exists full_path then
            Stdio.In_channel.read_all full_path
            |> parse_func
            |> Result.map_error ~f:(fun e -> Error.of_string @@ Frontend.show_parseError e)
            |> Result.map ~f:(fun expr ->
                   { Imports.full_path
                   ; expr=
                       ( match Ast.Sexp_ref.( ! ) expr with
                       | Ast.Module m -> Ast.Make.moduleNode (List.last_exn path) m.lines
                       | _ -> expr ) })
          else find_result rest in
    find_result ["../../../examples/"; "../../../examples/stdlib/"] in
  (* To run import resolution, we start from the main file and run load_import recursively against
     all its imports *)
  let%bind.Result expr = parse_func src in
  let init = {Imports.main= expr; Imports.imports= Map.empty (module Ast)} in
  let f (imports : Imports.t) import_node : Imports.t =
    match Ast.Sexp_ref.( ! ) import_node with
    | Ast.Import {path; _} -> (
      match load_import ~path with
      | Error e -> failwith @@ Error.to_string_hum e
      | Ok resolved -> Imports.add imports import_node resolved.expr )
    | _ -> imports in
  Ok (Ast.fold ~init ~f expr)
