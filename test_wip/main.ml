open Coral_frontend
open Base

let check_source_compiles source =
  match Frontend.parse_string source with
  | Ok e ->
      let e = Coral.Init_func.run e in
      let _imports = Coral.Import_resolution.resolve e in
      let ns = Coral.Name_resolution.resolve e in
      let _ts = Coral_types.Resolver.resolve ns e in
      let ns = Coral_types.Apply_overload_reference.fix_name_resolution ns _ts in
      let _ = Coral.Llvm_backend.print_ir ns _ts e in
      (* Stdio.print_endline @@ Ast.show_node e; *)
      true
  | Error e ->
      Stdio.eprintf "[%s]" (Frontend.show_parseError e);
      false
  | exception _exn ->
    Stdio.print_endline @@ Exn.to_string _exn;
    false
let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

let%test "wip" =
  Stdio.print_endline @@ Unix.getcwd();
  check_file_compiles "../examples/wip.coral"
