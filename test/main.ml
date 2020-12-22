
let compiles source =
  match 
    Coral.Import_resolution.resolve
      ~parse_func:Coral_frontend.Frontend.parse_string
      source with
  | Ok e ->
    let e = Coral.Init_func.run e in
    let _ns = Coral.Name_resolution.resolve e in
    let _ts = Coral_types.Resolver.resolve _ns e in
    let _ns = Coral_types.Apply_overload_reference.fix_name_resolution _ns _ts in
    let _ = Coral.Llvm_backend.print_ir _ns _ts e in
    true
  | Error e ->
    Stdio.printf "%s" (Coral_frontend.Frontend.show_parseError e);
    false

let parses source =
  match Coral_frontend.Frontend.parse_string source with
  | Ok _ ->
    true
  | Error e ->
    Stdio.printf "%s" (Coral_frontend.Frontend.show_parseError e);
    false

let check_source compiles source =
  compiles source

let check_file compiles path =
  let source = Stdio.In_channel.read_all path in
  compiles source

let%test "hello world" = check_source compiles {|
extern("c", "printf", Func[][Str, ...])
printf "Hello, World!\n"|}

let%test "fibonacci" =
  check_source parses
    {|
extern("c", "printf", Func[][Str, ...])

func fib(n):
  if n <= 1:
    return 1
  else:
    return fib(n - 1) + fib(n - 2)
# here we check that trailing dedents are handled correctly as well
if 1:
  printf("%d\n", fib 7)|}

let%test "fizzbuzz" =
  check_source compiles
    {|
extern("c", "printf", Func[][Str, ...])
func fizzbuzz(n:Int64):
  if n % 15 = 0: return printf "Fizzbuzz "
  elif n % 3 = 0: return printf "Fizz "
  elif n % 5 = 0: return printf "Buzz "
  else: return printf("%d ", n)
fizzbuzz 10
fizzbuzz 11
fizzbuzz 12
fizzbuzz 13
fizzbuzz 14
fizzbuzz 15
printf "\n"
|}

let%test "fasta" =
  check_file parses "../examples/benchmarks_game/fasta.coral"

let%test "pidigits" =
  check_file parses "../examples/benchmarks_game/pidigits.coral"

let%test "knucleotide" =
  check_file parses "../examples/benchmarks_game/knucleotide.coral"

let%test "regex-redux" =
  check_file parses "../examples/benchmarks_game/regex-redux.coral"

let%test "sys-io.unix" =
  check_file parses "../examples/sys_io.posix.coral"

let%test "indents" =
  check_source parses
    {|
func digits():
  if 1:
    foo
  else:
    bar
  foo
|}
  &&
  check_source parses {|func digits():
  foo |}

let%test "overload-builtins" =
  check_source compiles {|
extern("c", "printf", Func[][Str, ...])
printf("%f, %d, %s\n", 0.5 + 3.3, 1 + 3, "thirtyfive")
|}
