let check_source_compiles source =
  let module Frontend = Coral_frontend.Frontend in
  match Frontend.parse_string source with
  | Ok _ -> true
  | Error e ->
    Printf.printf "%s"  (Frontend.show_parseError e)
    ;false

let check_file_compiles path =
  let source = Stdio.In_channel.read_all path in
  check_source_compiles source

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


let%test "fasta" = check_file_compiles "../../../examples/benchmarks_game/fasta.coral"
let%test "pidigits" = check_file_compiles "../../../examples/benchmarks_game/pidigits.coral"
let%test "knucleotide" = check_file_compiles "../../../examples/benchmarks_game/knucleotide.coral"
let%test "regex-redux" = check_file_compiles "../../../examples/benchmarks_game/regex-redux.coral"
