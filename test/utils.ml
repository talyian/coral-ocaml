
let dump_expr expr =
  Coral_core.Ast.show_node expr |> print_endline

let parse_with_imports str =
  Coral_core.Ast.Info._id := 0;
  Coral_frontend.Import_resolution.resolve ~parse_func:Coral_frontend.Frontend.parse_string ~src:str
  (* let lexbuf = Lexing.from_string str in
   * let lexer = Coral_frontend.LexerInterface.create_tokenizer () in
   * let (Coral_frontend.Grammar_helper.Main expr) = Coral_frontend.Grammar.main lexer lexbuf in
   * expr *)

let clean_vt_escape s =
  Re.replace_string ~by:"" (Re.Pcre.regexp "\x1b\\[.*?m") s
