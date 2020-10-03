open Coral_core
open Coral_frontend

let check_source_compiles source =
  match Frontend.parse_string source with
  | Ok e -> print_endline @@ Ast.show_node e; true
  | Error e ->
      Printf.eprintf "[%s]" (Frontend.show_parseError e);
      false

let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

let%test "wip" =
  (* why is pwd the root directory but Stdio.Inchannel reads relative to current file path? *)
  Sys.getenv "PWD" |> Stdio.printf " [pwd] = %s\n";
  check_file_compiles "../../../../../examples/wip.coral"
