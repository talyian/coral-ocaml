open Coral_core
open Coral_frontend
open Base

let check_source_compiles source =
  match Frontend.parse_string source with
  | Ok e ->
      let _ = Coral.Import_resolution.resolve e in
      let e = Coral.Name_resolution.resolve e in
      let _ = Coral.Llvm_backend.print_ir e in
      (* Stdio.print_endline @@ Ast.show_node e; *)
      true
  | Error e ->
      Stdio.eprintf "[%s]" (Frontend.show_parseError e);
      false

let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

let%test "wip" = check_file_compiles "../../../../../examples/wip.coral"
