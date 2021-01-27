open Base
open Coral_core
open Coral_frontend

let test_source src =
  (match
     let%bind.Result imports = Utils.parse_with_imports src in
     let names = Coral_passes.Name_resolution.(get_data @@ construct imports) in
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
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (Failure
    "unhandled node type in check_type: ref (List {items = [ref (Var {name = \"Uint8\"; info = ()})]; info = ()})")
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Base__List.fold_map.(fun) in file "src/list.ml", line 432, characters 23-31
  Called from Base__List.count_map in file "src/list.ml", line 387, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 418, characters 15-31
  Called from Base__List.fold_map in file "src/list.ml", line 431, characters 4-88
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 65, characters 26-66
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Base__List.fold_map.(fun) in file "src/list.ml", line 432, characters 23-31
  Called from Base__List.count_map in file "src/list.ml", line 390, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 418, characters 15-31
  Called from Base__List.fold_map in file "src/list.ml", line 431, characters 4-88
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 65, characters 26-66
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 83, characters 24-40
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 64, characters 27-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 50, characters 25-42
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 42, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 64, characters 27-46
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
  Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 9, characters 17-66
  Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-227
  Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 19, characters 2-143
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
;;
let%expect_test "types - fizzbuzz" =
  test_source {|
func fizzbuzz(n):
  n

func main():
  fizzbuzz 20
|};
  [%expect {|
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

func main(): ()
|};
  [%expect {|
    Func-main: (Applied ((Applied ((Const FUNC), [(Const VOID)])), []))
    (): (Const VOID) |}]
