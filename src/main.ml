open Foob
open Lexer
open Printf

let print_tokens lexbuf =
  let rec loop () =
    print_string "\n";
    let token = Lexer.coral_token lexbuf in
    match token with
    | INTEGER (n) -> print_int n; loop ()
    | OPERATOR (n) -> print_string n; loop()
    | IDENTIFIER (s) -> print_string s; loop()
    | OTHER(c) -> print_char c; loop()
    | STRING(s) -> printf "\027[38;5;123m\"%s\"\027[0m" s; loop()
    | EOF -> ()
    | FUNC -> loop()
    | IF -> loop()
    | ELSE -> loop()
    | _ -> print_string "unhandled token"
  in loop ()

let () =
  let f = open_in "samples/factorial.coral" in
  let lexbuf = Lexing.from_channel f in
  let result = Grammar.main Lexer.coral_token lexbuf in print_int result;
  print_tokens lexbuf
