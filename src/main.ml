open Coral_frontend
open Base

let check_source_compiles src =
  let _oo = Coral.Import_resolution.resolve ~parse_func:Frontend.parse_string ~src in
  match _oo with
  | Ok e ->
      let e = Coral.Init_func.run e in
      let _imports = Coral.Import_resolution.resolve e in
      let _ns = Coral.Name_resolution.resolve e in
      let _ts = Coral_types.Resolver.resolve _ns e in
      let _ns = Coral_types.Apply_overload_reference.fix_name_resolution _ns _ts in
      (* let _ = Coral.Type_resolution_3.resolve _ns e in *)
      (* let ts = Coral.Type_resolution.resolve e in *)
      let _ = Coral.Llvm_backend.print_ir _ns _ts e in
      (* Stdio.print_endline @@ Ast.show_node e; *)
      true
  | Error e ->
      Stdio.eprintf "[%s]" (Frontend.show_parseError e) ;
      false

let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

let _ = check_file_compiles "examples/wip.coral"
