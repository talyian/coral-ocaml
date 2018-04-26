open Printf
open Grammar
open LlvmBackend

open OUnit

let parse lexbuf =
  let m =
    try
      Grammar.main LexerInterface.token lexbuf
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
  |> Type_resolver.run

let parse_file filename = open_in filename |> Lexing.from_channel |> parse

let parse_string x = Lexing.from_string x |> parse

let run = LlvmBackend.run

let show m =
  Ast.show m
