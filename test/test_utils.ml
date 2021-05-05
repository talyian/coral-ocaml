open Base

let dump_expr expr =
  Coral_core.Ast.show_node expr |> Stdio.print_endline

let parse_with_imports str =
  Coral_frontend.Import_resolution.resolve ~parse_func:Coral_frontend.Frontend.parse_string ~src:str

let parse_file_with_imports filepath =
  let filepath = "../../../" ^ filepath in
  let src = Stdio.In_channel.read_all filepath in
  Coral_frontend.Import_resolution.resolve ~parse_func:Coral_frontend.Frontend.parse_string ~src
let clean_vt_escape s =
  Re.replace_string ~by:"" (Re.Pcre.regexp "\x1b\\[.*?m") s
