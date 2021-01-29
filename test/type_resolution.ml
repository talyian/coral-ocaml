open Base
open Coral_core
open Coral_frontend

let test_source src =
  (match
     let%bind.Result imports = Utils.parse_with_imports src in
     let names = Coral_passes.Name_resolution.construct imports in
     Coral_passes.Name_resolution.show names;
     let names = Coral_passes.Name_resolution.get_data names in
     let types = Coral_types.Resolver.construct names imports.main in
     Ok types
  with
  | Ok types ->
    Coral_types.Resolver.Resolver.dump types
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e)

let test_file filename = Stdio.In_channel.read_all filename |> test_source

let%expect_test "types - hello world" =
  test_source {|
import raw_clib (printf)

func foo(world):
  printf("Hello, %s\n", world)

func main():
  let target = "world"
  foo target
|};
  [%expect{|
    Names
        [Var-...] -> Builtin-ELLIPSIS
        [Var-Cstr] -> Let-Cstr
        [Var-Func] -> Builtin-FUNC
        [Var-Ptr] -> Builtin-PTR
        [Var-Uint64] -> Builtin-UINT64
        [Var-Uint8] -> Builtin-UINT8
        [Var-foo] -> Func-foo
        [Var-printf] -> Extern-printf
        [Var-target] -> Let-target
        [Var-world] -> Param-world
    Names.members
        [Module.().foo] -> Func-foo
        [Module.().main] -> Func-main
        [Module.().printf] -> Extern-printf
        [raw_clib.Cstr] -> Let-Cstr
        [raw_clib.free] -> Extern-free
        [raw_clib.malloc] -> Extern-malloc
        [raw_clib.printf] -> Extern-printf
    call of func type:
      ::FUNC[][PTR[UINT8], ELLIPSIS][::STR, *]
      *
      ::STR
    TODO: unify args and params
    Call-Func        FUNC[][PTR[UINT8], ELLIPSIS]
    Call-Func        FUNC[]
    Call-foo         ::FUNC[][PTR[UINT8], ELLIPSIS][::STR, *]
    Call-printf      ::FUNC[][PTR[UINT8], ELLIPSIS][::STR, *]
    Extern-printf    ::FUNC[][PTR[UINT8], ELLIPSIS]
    Func-foo         FUNC[::FUNC[][PTR[UINT8], ELLIPSIS][::STR, *]][*]
    Func-main        FUNC[::VOID][]
    Param-world      *
    "Hello, %s\n"    ::STR
    "world"          ::STR
    Var-...          ELLIPSIS
    Var-Cstr         PTR[UINT8]
    Var-Func         FUNC
    Var-Ptr          PTR
    Var-Uint8        UINT8
    Var-foo          FUNC[::FUNC[][PTR[UINT8], ELLIPSIS][::STR, *]][*]
    Var-printf       ::FUNC[][PTR[UINT8], ELLIPSIS]
    Var-target       ::STR
    Var-world        *
    Let-Cstr         PTR[UINT8]
    Let-target       ::STR
    Block            ::VOID
    expr-EXPR        PTR[UINT8]
    Builtin-UINT8    UINT8
    Builtin-PTR      PTR
    Builtin-FUNC     FUNC
    Builtin-ELLIPSIS ELLIPSIS |}]
;;
let%expect_test "types - fizzbuzz" =
  test_source {|
func fizzbuzz(n):
  n

func main():
  fizzbuzz 20
|};
  [%expect {|
    Names
        [Var-fizzbuzz] -> Func-fizzbuzz
        [Var-n] -> Param-n
    Names.members
        [Module.().fizzbuzz] -> Func-fizzbuzz
        [Module.().main] -> Func-main
    call of func type:
      *
      *
      ::INT64
    TODO: unify args and params
    Call-fizzbuzz *
    Func-fizzbuzz FUNC[*][*]
    Func-main     FUNC[*][]
    Param-n       *
    expr-EXPR     ::INT64
    Var-fizzbuzz  FUNC[*][*]
    Var-n         * |}]

let%expect_test "types - libs" =
  test_source {|
import raw_clib
import raw_posix

func main():
  raw_clib.printf("Hello, %s\n", "World")
  raw_posix.open("/etc/passwd", 0)
|};
  [%expect{|
    Names
        [Var-...] -> Builtin-ELLIPSIS
        [Var-Cstr] -> Let-Cstr
        [Var-Fd] -> expr-EXPR
        [Var-Flags] -> expr-EXPR
        [Var-Func] -> Builtin-FUNC
        [Var-Int32] -> Builtin-INT32
        [Var-IntSize] -> Builtin-INTNATIVE
        [Var-Ptr] -> Builtin-PTR
        [Var-Uint64] -> Builtin-UINT64
        [Var-Uint8] -> Builtin-UINT8
        [Var-UintSize] -> Builtin-INTNATIVE
        [Var-raw_clib] -> raw_clib
        [Var-raw_posix] -> raw_posix
    Names.members
        [Module.().main] -> Func-main
        [Module.().raw_clib] -> raw_clib
        [Module.().raw_posix] -> raw_posix
        [raw_clib.Cstr] -> Let-Cstr
        [raw_clib.free] -> Extern-free
        [raw_clib.malloc] -> Extern-malloc
        [raw_clib.printf] -> Extern-printf
        [raw_posix.Fd] -> expr-EXPR
        [raw_posix.Flags] -> expr-EXPR
        [raw_posix.close] -> Extern-close
        [raw_posix.dup] -> Extern-dup
        [raw_posix.open] -> Extern-open
        [raw_posix.read] -> Extern-read
    Call-Func        FUNC[][PTR[UINT8], ELLIPSIS]
    Call-Func        FUNC[Fd][PTR[UINT8], INT32]
    Call-Func        FUNC[]
    Call-Func        FUNC[Fd]
    Call-Ptr         PTR[UINT8]
    Call-EXPR        ::FUNC[][PTR[UINT8], ELLIPSIS][::STR, ::STR]
    Call-EXPR        ::FUNC[Fd][PTR[UINT8], INT32][::STR, ::INT64]
    Extern-open      ::FUNC[Fd][PTR[UINT8], INT32]
    Extern-printf    ::FUNC[][PTR[UINT8], ELLIPSIS]
    Func-main        FUNC[::VOID][]
    expr-EXPR        ::INT64
    "/etc/passw"     ::STR
    "Hello, %s\n"    ::STR
    "World"          ::STR
    Var-...          ELLIPSIS
    Var-Cstr         PTR[UINT8]
    Var-Fd           Fd
    Var-Flags        INT32
    Var-Func         FUNC
    Var-Int32        INT32
    Var-Ptr          PTR
    Var-Uint8        UINT8
    Let-Cstr         PTR[UINT8]
    Block            ::VOID
    expr-EXPR        PTR[UINT8]
    expr-EXPR        ::FUNC[][PTR[UINT8], ELLIPSIS]
    expr-EXPR        ::FUNC[Fd][PTR[UINT8], INT32]
    Builtin-INT32    INT32
    Builtin-UINT8    UINT8
    Builtin-PTR      PTR
    Builtin-FUNC     FUNC
    Builtin-ELLIPSIS ELLIPSIS
    expr-EXPR        Fd
    expr-EXPR        INT32 |}]

let%expect_test "test - basic inference" =
  test_source {|
func foo(w:Int64):
  3.4

func main():
  let x = 3
  let y = x
  let z = foo 3
|};
  [%expect {|
    Names
        [Var-Int64] -> Builtin-INT64
        [Var-foo] -> Func-foo
        [Var-x] -> Let-x
    Names.members
        [Module.().foo] -> Func-foo
        [Module.().main] -> Func-main
    call of func type:
      ::FLOAT64
      ::INT64
      ::INT64
    TODO: unify args and params
    Call-foo      ::FLOAT64
    Func-foo      FUNC[::FLOAT64][::INT64]
    Func-main     FUNC[::VOID][]
    Param-w       ::INT64
    expr-EXPR     ::INT64
    expr-EXPR     ::FLOAT64
    Var-Int64     INT64
    Var-foo       FUNC[::FLOAT64][::INT64]
    Var-x         ::INT64
    Let-x         ::INT64
    Let-y         ::INT64
    Let-z         ::FLOAT64
    Block         ::VOID
    Builtin-INT64 INT64 |}]
