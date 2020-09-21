open Lexing
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
    | FLOAT f -> print_string f; loop ctx
    | IDENTIFIER (s) -> print_string s; loop ctx
    | OTHER(c) -> print_string "OTHER "; print_char c; loop ctx
    | STRING(s) -> printf "\027[38;5;123m\"%s\"\027[0m" s; loop ctx
    | EOF -> ()
    | FUNC -> printf "FUNC "; loop ctx
    | IF -> printf "IF "; loop ctx
    | ELSE -> printf "ELSE "; loop ctx
    | COLON -> printf ":"; loop ctx
    | NEWLINE _ -> printf "NEWLINE"; loop ctx
    | INDENT -> printf "INDENT"; loop ctx
    | DEDENT -> printf "DEDENT"; loop ctx
    | LPAREN -> printf "LPAREN ("; loop ctx
    | RPAREN -> printf "RPAREN )"; loop ctx
    | COMMA -> printf "COMMA"; loop ctx
    | ELLIPSIS -> printf "ELLIPSIS"; loop ctx
    | RETURN -> printf "RETURN"; loop ctx
    | OPERATOR (n) -> print_string n; loop ctx
    | OPERADD (n) -> print_string n; loop ctx
    | OPERMUL (n) -> print_string n; loop ctx
    | OPERCMP (n) -> print_string n; loop ctx
    | EQ -> print_string "="; loop ctx
    | _ -> printf "unhandled token `%s`" (Lexing.lexeme lexbuf)
  in loop {tokenqueue=[]; indents=[0]; nestlevel=0}

let token = expand_tokens Lexer.coral_token {tokenqueue=[]; indents=[0]; nestlevel=0}
