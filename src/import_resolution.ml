open Coral_core

(** To import a module, [import Foo.Bar] will find the file "foo/bar.coral"
relative to a system import-root setting.

 ** Paths:
 *** Absolute imports
 **** System imports
 **** Project dependency imports
 *** Relative imports

 ** which imports
*)

(* The two primary places to search for an import file is either a system-level import root
   or a project-level import root *)
module type Settings = sig
  val import_root : string

  val project_import_root : string
end

type path =
  (* TODO: uri imports a la Go *)
  | AbsolutePath of string list
  | RelativePath of string list

let try_read_file path =
  try Some (Stdio.In_channel.read_all path) with _ -> None

let rec resolve expr =
  match expr with
  | Ast.Import import, _ ->
      Stdio.printf "IMPORT\n";
      let filename = String.concat "/" import.path ^ ".coral" in
      let _found_module =
        Base.List.find_map
          ~f:(fun parent_path -> try_read_file @@ parent_path ^ filename)
          [ "../../../examples"; "../../../examples/stdlib" ]
      in
      (* find filename relative to system import-root *)
      (* find filename relative to project root. if importing module is a script, project root is its parent folder *)
      ()
  | _ -> Ast.iter resolve expr
