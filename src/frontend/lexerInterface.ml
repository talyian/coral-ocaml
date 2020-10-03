open Lexer

let rec expand_tokens lex_function ctx lexbuf =
  match ctx.tokenqueue with
  | tok :: xs ->
      ctx.tokenqueue <- xs;
      (* print_endline @@ Token.show_token tok; *)
      tok
  | [] ->
      ctx.tokenqueue <- lex_function ctx lexbuf;
      expand_tokens lex_function ctx lexbuf

let print_tokens lexbuf =
  let rec loop ctx =
    match expand_tokens Lexer.coral_token ctx lexbuf with
    | Token.EOF -> ()
    | t ->
        print_endline @@ Token.show_token t;
        loop ctx
  in
  loop { tokenqueue = []; indents = [ 0 ]; nestlevel = 0 }

let create_tokenizer () =
  let state = { tokenqueue = []; indents = [ 0 ]; nestlevel = 0 } in
  fun buf -> expand_tokens Lexer.coral_token state buf
