
let dump_expr expr =
  Coral_core.Ast.show_node expr |> print_endline

let parse_with_imports str =
  Coral_frontend.Import_resolution.resolve ~parse_func:Coral_frontend.Frontend.parse_string ~src:str

let clean_vt_escape s =
  Re.replace_string ~by:"" (Re.Pcre.regexp "\x1b\\[.*?m") s
