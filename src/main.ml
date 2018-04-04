open Printf
open Lexing

open Lexer
open Grammar

let rec expand_tokens ctx lexbuf =
  match ctx.tokenqueue with
  | [] -> ctx.tokenqueue <- Lexer.coral_token ctx lexbuf; expand_tokens ctx lexbuf
  | tok :: xs -> ctx.tokenqueue <- xs; tok


let tokens lexbuf =
  let rec loop ctx =
    print_string "\n";
    match expand_tokens ctx lexbuf with
      | INTEGER (n) -> print_int n; loop ctx
      | OPERATOR (n) -> print_string n; loop ctx
      | IDENTIFIER (s) -> print_string s; loop ctx
      | OTHER(c) -> print_string "OTHER "; print_char c; loop ctx
      | STRING(s) -> printf "\027[38;5;123m\"%s\"\027[0m" s; loop ctx
      | EOF -> ()
      | FUNC -> printf "FUNC "; loop ctx
      | IF -> printf "if "; loop ctx
      | ELSE -> printf "else "; loop ctx
      | COLON -> printf ":"; loop ctx
      | NEWLINE(n) ->
         printf "NEWLINE";
         loop ctx
      | INDENT -> printf "INDENT"; loop ctx
      | DEDENT -> printf "DEDENT"; loop ctx
      | LPAREN -> printf "LPAREN ("; loop ctx
      | RPAREN -> printf "RPAREN )"; loop ctx
      | _ -> printf "unhandled token `%s`" (Lexing.lexeme lexbuf)
  in loop {tokenqueue=[]; indents=[0]}

let parse lexbuf =
  try
    Grammar.main (expand_tokens {tokenqueue=[]; indents=[0]}) lexbuf
  with exc ->
    printf "\027[1;31mError (%s)\027[0m\n" (Lexing.lexeme lexbuf);
    let pos = lexbuf.lex_start_p in
    printf "%d(%d:%d)" pos.pos_cnum pos.pos_lnum pos.pos_bol;
    0

let () =
  let f = open_in "samples/factorial.coral" in
  let lexbuf = Lexing.from_channel f in
  ignore (parse lexbuf)
