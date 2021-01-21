open Coral_frontend
open Coral_passes
open Base

let check_source_compiles src =
  let oo = Import_resolution.resolve ~parse_func:Frontend.parse_string ~src in
  match oo with
  | Ok imported ->
      let e = imported.main in
      let names = Name_resolution.resolve imported in
      (* let e = Coral.Init_func.run e in
       * let _imports = Coral.Import_resolution.resolve e in
       * let _ns = Coral.Name_resolution.resolve e in
       * let _ts = Coral_types.Resolver.resolve _ns e in
       * let _ns = Coral_types.Apply_overload_reference.fix_name_resolution _ns _ts in
       * (\* let _ = Coral.Type_resolution_3.resolve _ns e in *\)
       * (\* let ts = Coral.Type_resolution.resolve e in *\)
       * let _ = Coral.Llvm_backend.print_ir _ns _ts e in *)
      (* Stdio.print_endline @@ Ast.show_node e; *)
      Name_resolution.show names ; true
  | Error e ->
      Stdio.eprintf "[%s]" (Frontend.show_parseError e) ;
      false

let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

let () =
  let open Core_kernel in
  Command.basic ~summary:"Coral compiler"
    (let%map_open.Core_kernel.Command filename = anon (maybe ("filename" %: string)) in
     fun () ->
       match filename with
       | Some f -> check_file_compiles f |> ignore
       | None -> check_source_compiles @@ In_channel.input_all Stdio.stdin |> ignore)
  |> Core.Command.run
