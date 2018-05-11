open Printf
open Grammar
open LlvmBackend

open OUnit

let parse name lexbuf =
  let m =
    try
      Grammar.main LexerInterface.token lexbuf
    with
    | Error as exc ->
       printf "\027[1;31mError (%s)\027[0m\n" (Lexing.lexeme lexbuf);
       let pos = lexbuf.lex_start_p in
       printf "%d(%d:%d)\n" pos.pos_cnum pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
       raise exc
  in (match m with | Module modinfo -> Ast.Module {modinfo with name=name} | x -> x)
  |> Multifunc.run
  |> Return_insert.run
  |> Init_func.run
  |> Name_resolver.run
  |> Type_resolver.run
  (* |> Ansicolor.tee (fun x -> Ast.show x; flush stdout; x) *)

let tokenize_file filename =
  open_in filename |> Lexing.from_channel |> LexerInterface.print_tokens

let parse_file filename = open_in filename |> Lexing.from_channel |> parse filename

let parse_string x = Lexing.from_string x |> parse "module"

let run = LlvmBackend.run

let show m =
  Ast.show m
