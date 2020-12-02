open Coral_frontend
open Base

let check_source_compiles source =
  try
    match Frontend.parse_string source with
    | Ok e ->
      let e = Coral.Init_func.run e in
      let _imports = Coral.Import_resolution.resolve e in
      let ns = Coral.Name_resolution.resolve e in
      let _ts = Coral_types.Resolver.resolve ns e in
      let ns = Coral_types.Apply_overload_reference.fix_name_resolution ns _ts in
      let _ = Coral.Llvm_backend.print_ir ns _ts e in
      true
    | Error e ->
      Stdio.eprintf "%s" (Frontend.show_parseError e);
      false
  with  _exn -> 
    Stdio.print_endline @@ Caml.Printexc.get_backtrace ();
    false

let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

let%test "wip" =
  check_file_compiles "../examples/wip.coral"
