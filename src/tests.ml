
let check_source_compiles source = 
  let module Frontend = Coral_frontend in
  let lexbuf = Lexing.from_string ~with_positions:true source in
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p
    with pos_cnum = 0; pos_lnum = 0; pos_bol = 0;
  };
  try 
    let token = Frontend.LexerInterface.create_tokenizer () in
    let node = Frontend.Grammar.main token lexbuf in
    true
  with
  | Frontend.Grammar.Error ->
    Stdio.printf "Parse Error at location:[%d:%d-%d] lexeme:[%s]\n"
      (Lexing.lexeme_start_p lexbuf).pos_lnum
      (Lexing.lexeme_start_p lexbuf).pos_cnum
      (Lexing.lexeme_end_p lexbuf).pos_cnum
      (Lexing.lexeme lexbuf);
    let find_line start _end =
      let rec find_start s = 
        if s = 0 then 0 
        else if String.get source s = '\n' then s + 1
        else find_start (s - 1)
      in
      let rec find_end e =
        if e = String.length source - 1 then e
        else if String.get source e = '\n' then e
        else find_end (e + 1) in
      let s = find_start start in
      let e = find_end _end in 
      Printf.printf "%s\x1b[1;31m%s\x1b[0m%s\n%s%s\x1b[0m\n"
        (String.sub source s (start - s))
        (String.sub source start (_end - start))
        (String.sub source _end (e - _end))
        (String.make (start - s) ' ')
        (String.make (_end - start) '^')
    in find_line (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf);
    false

let%test "hello world" = check_source_compiles {|printf "Hello, World!\n"|} 
let%test "fibonacci" = check_source_compiles {|
func fib(n):
  if n <= 1: 
    return 1
  else: 
    return fib(n - 1) + fib(n - 2)
# here we check that trailing dedents are handled correctly as well
if True:
  printf("%d\n", fib 7)|}
let%test "fizzbuzz" = check_source_compiles {|
func fizzbuzz(n): 
  if n % 15 = 0: printf "Fizzbuzz "
  elif n % 3 = 0: printf "Fizz "
  elif n % 5 = 0: printf "Buzz "
  else: printf("%d ", n)
fizzbuzz 10
fizzbuzz 11
fizzbuzz 12
fizzbuzz 13
fizzbuzz 14
fizzbuzz 15
|} 
