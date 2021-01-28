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
      (Applied (
       (InstanceOf
          (Applied ((Applied ((Const FUNC), [])),
             [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)]))),
       [(Const STR); Any]))
      Any
      (Const STR)
    Call-Func: (Applied ((Applied ((Const FUNC), [])),
       [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)]))
    Call-Func: (Applied ((Const FUNC), []))
    Call-foo: (Applied (
       (InstanceOf
          (Applied ((Applied ((Const FUNC), [])),
             [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)]))),
       [(Const STR); Any]))
    Call-printf: (Applied (
       (InstanceOf
          (Applied ((Applied ((Const FUNC), [])),
             [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)]))),
       [(Const STR); Any]))
    Extern-printf: (InstanceOf
       (Applied ((Applied ((Const FUNC), [])),
          [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)])))
    Func-foo: (Applied (
       (Applied ((Const FUNC),
          [(Applied (
              (InstanceOf
                 (Applied ((Applied ((Const FUNC), [])),
                    [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)]
                    ))),
              [(Const STR); Any]))
            ]
          )),
       [Any]))
    Func-main: (Applied ((Applied ((Const FUNC), [(Const VOID)])), []))
    Param-world: Any
    "Hello, %s\n": (Const STR)
    "world": (Const STR)
    Var-...: (Const ELLIPSIS)
    Var-Cstr: (Applied ((Const PTR), [(Const UINT8)]))
    Var-Func: (Const FUNC)
    Var-Ptr: (Const PTR)
    Var-Uint8: (Const UINT8)
    Var-foo: (Applied (
       (Applied ((Const FUNC),
          [(Applied (
              (InstanceOf
                 (Applied ((Applied ((Const FUNC), [])),
                    [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)]
                    ))),
              [(Const STR); Any]))
            ]
          )),
       [Any]))
    Var-printf: (InstanceOf
       (Applied ((Applied ((Const FUNC), [])),
          [(Applied ((Const PTR), [(Const UINT8)])); (Const ELLIPSIS)])))
    Var-target: (Const STR)
    Var-world: Any
    Let-Cstr: (Applied ((Const PTR), [(Const UINT8)]))
    Let-target: (Const STR)
    Block: (Const VOID)
    expr-EXPR: (Applied ((Const PTR), [(Const UINT8)]))
    Builtin-UINT8: (Const UINT8)
    Builtin-PTR: (Const PTR)
    Builtin-FUNC: (Const FUNC)
    Builtin-ELLIPSIS: (Const ELLIPSIS) |}]
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
      Any
      Any
      (Const INT64)
    Call-fizzbuzz: Any
    Func-fizzbuzz: (Applied ((Applied ((Const FUNC), [Any])), [Any]))
    Func-main: (Applied ((Applied ((Const FUNC), [Any])), []))
    Param-n: Any
    expr-EXPR: (Const INT64)
    Var-fizzbuzz: (Applied ((Applied ((Const FUNC), [Any])), [Any]))
    Var-n: Any |}]

let%expect_test "types - libs" =
  test_source {|
import raw_clib
import raw_posix

func main():
  raw_clib.printf("Hello, %s\n", "World")
  raw_posix.open("/etc/passwd", 0)
|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure
     "unhandled node type in check_type: ref (TypeDecl {name = \"Fd\"; metatype = \"struct\";\
    \n       fields = [ref (Empty {info = ()})]; info = ()})")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Base__List.fold_map.(fun) in file "src/list.ml", line 432, characters 23-31
  Called from Base__List.count_map in file "src/list.ml", line 387, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 418, characters 15-31
  Called from Base__List.fold_map in file "src/list.ml", line 431, characters 4-88
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 67, characters 26-66
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 66, characters 27-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 85, characters 24-40
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 66, characters 27-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Base__List.fold_map.(fun) in file "src/list.ml", line 432, characters 23-31
  Called from Base__List.count_map in file "src/list.ml", line 391, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 418, characters 15-31
  Called from Base__List.fold_map in file "src/list.ml", line 431, characters 4-88
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 55, characters 26-67
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 50, characters 25-42
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.construct in file "src/type_resolution/resolver.ml", line 34, characters 9-112
  Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 11, characters 17-66
  Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-323
  Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 148, characters 2-143
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
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
      [raw_posix.read] -> Extern-read |}]
