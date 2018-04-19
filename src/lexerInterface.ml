open Lexing
open Ast
open Lexer
open Printf
       
let rec expand_tokens lex_function ctx lexbuf =
  match ctx.tokenqueue with
  | tok :: xs ->
     ctx.tokenqueue <- xs;
     tok
  | [] ->
     ctx.tokenqueue <- lex_function ctx lexbuf;
     expand_tokens lex_function ctx lexbuf

let print_tokens lexbuf =
  let rec loop ctx =
    printf "\n %2d:%-2d " lexbuf.lex_start_p.pos_lnum lexbuf.lex_start_p.pos_bol;
    match expand_tokens Lexer.coral_token ctx lexbuf with
      | INTEGER (n) -> print_string n; loop ctx
      | OPERATOR (n) -> print_string n; loop ctx
      | IDENTIFIER (s) -> print_string s; loop ctx
      | OTHER(c) -> print_string "OTHER "; print_char c; loop ctx
      | STRING(s) -> printf "\027[38;5;123m\"%s\"\027[0m" s; loop ctx
      | EOF -> ()
      | FUNC -> printf "FUNC "; loop ctx
      | IF -> printf "if "; loop ctx
      | ELSE -> printf "else "; loop ctx
      | COLON -> printf ":"; loop ctx
      | NEWLINE(n) -> printf "NEWLINE"; loop ctx
      | INDENT -> printf "INDENT"; loop ctx
      | DEDENT -> printf "DEDENT"; loop ctx
      | LPAREN -> printf "LPAREN ("; loop ctx
      | RPAREN -> printf "RPAREN )"; loop ctx
      | COMMA -> printf "COMMA"; loop ctx
      | ELLIPSIS -> printf "ELLIPSIS"; loop ctx
      | RETURN -> printf "RETURN"; loop ctx
      | _ -> printf "unhandled token `%s`" (Lexing.lexeme lexbuf)
  in loop {tokenqueue=[]; indents=[0]}

let token = expand_tokens Lexer.coral_token {tokenqueue=[]; indents=[0]}
              
