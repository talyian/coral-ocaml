
open Coral_core
open Base

let%expect_test "knucleotide" =
  match
    let filename = "knucleotide.coral" in
    let src = Stdio.In_channel.read_all "../../../examples/benchmarks_game/regex-redux.coral" in
    let%bind.Result imports = Coral_frontend.Import_resolution.resolve
        ~parse_func:Coral_frontend.Frontend.parse_string
        ~src in
    let names = Coral_passes.Name_resolution.construct imports in
    let name_data = Coral_passes.Name_resolution.get_data names in
    let type_info = Coral_passes.Type_visitor.construct name_data imports.main in
    Ok () with
  | Ok _ ->
    [%expect.unreachable]
  | Error err ->
    Coral_frontend.Frontend.show_parseError err |> Stdio.print_endline
  | exception exc ->
    Exn.to_string exc |> Stdio.print_endline;
    [%expect {| |}]
