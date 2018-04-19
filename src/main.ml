open Printf
open Lexing
open Ast
open Lexer
open Grammar

open LlvmBackend

open OUnit

let rec expand_tokens ctx lexbuf =
  match ctx.tokenqueue with
  | [] -> ctx.tokenqueue <- Lexer.coral_token ctx lexbuf; expand_tokens ctx lexbuf
  | tok :: xs -> ctx.tokenqueue <- xs; tok

let tokens lexbuf =
  let rec loop ctx =
    printf "\n %2d:%-2d " lexbuf.lex_start_p.pos_lnum lexbuf.lex_start_p.pos_bol;
    match expand_tokens ctx lexbuf with
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
      | NEWLINE(n) ->
         printf "NEWLINE";
         loop ctx
      | INDENT -> printf "INDENT"; loop ctx
      | DEDENT -> printf "DEDENT"; loop ctx
      | LPAREN -> printf "LPAREN ("; loop ctx
      | RPAREN -> printf "RPAREN )"; loop ctx
      | COMMA -> printf "COMMA"; loop ctx
      | ELLIPSIS -> printf "ELLIPSIS"; loop ctx
      | RETURN -> printf "RETURN"; loop ctx
      | _ -> printf "unhandled token `%s`" (Lexing.lexeme lexbuf)
  in loop {tokenqueue=[]; indents=[0]}

let parse lexbuf =
  let m =
    try
      Grammar.main (expand_tokens {tokenqueue=[]; indents=[0]}) lexbuf
    with exc ->
      printf "\027[1;31mError (%s)\027[0m\n" (Lexing.lexeme lexbuf);
      let pos = lexbuf.lex_start_p in
      printf "%d(%d:%d)\n" pos.pos_cnum pos.pos_lnum pos.pos_bol;
      raise exc
  in m
  |> Multifunc.run
  |> Return_insert.run
  |> Init_func.run
  |> Name_resolver.run
  |> (fun m -> Ast.show m; m)
  |> Type_resolver.run

let parse_file filename = open_in filename |> Lexing.from_channel |> parse
let run = LlvmBackend.run

let () =
  let parse_test filename = parse_file filename |> ignore in
  let suite = "Tests" >::: [
    "Core" >::: [
      (* basics *)
      "hello_world" >:: (fun _ -> parse_test "samples/core/hello.coral");
      "factorial" >:: (fun _ -> parse_test "samples/core/factorial.coral");
      "collatz"  >:: (fun _ -> parse_test "samples/core/collatz.coral");
      (* inference *)
      "inference" >:: (fun _ -> parse_test "samples/core/inference.coral");
      "generics" >:: (fun _ -> parse_test "samples/core/tuples.coral");
      "polymorphism" >:: (fun _ -> parse_test "samples/core/polymorphism_adhoc.coral");
    ]
  ]
  in
  OUnit.run_test_tt suite;
  exit 0;
  (* let f = open_in "samples/polymorphism_adhoc.coral" in *)
  let f = open_in "samples/collatz.coral" in
  (* let f = open_in "samples/factorial.coral" in *)
  let lexbuf = Lexing.from_channel f in
  parse lexbuf |> run
