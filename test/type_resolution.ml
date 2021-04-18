open Base
open Coral_core
open Coral_frontend

let show_types source =
  match let%bind.Result imports = Utils.parse_with_imports source in
         let names = Coral_passes.Name_resolution.construct imports in
         let names = Coral_passes.Name_resolution.get_data names in
         let attributes, main = Coral_passes.Attribute_resolution.run imports.main in
         let types = Coral_types.Resolver.construct names main in
     Ok types with
  | Ok types ->
    Coral_types.Resolver.dump types;
  | Error e ->
    Stdio.print_endline @@ Frontend.show_parseError e;
  | exception e ->
    Stdio.print_endline @@ Exn.to_string e
;;

let%expect_test "types - hello world" =
  show_types {|
extern("c", "printf", Func[...][])

func main ():
  printf("Hello, %s\n", "World")
|};
    [%expect {|
      Call-Func        FUNC[ELLIPSIS][]
      Call-Func        FUNC[ELLIPSIS]
      Call-printf      ::VOID
      Extern-printf    ::FUNC[ELLIPSIS][]
      main             FUNC[][]
      "Hello, %s\n"    Hello, %s

      "World"          World
      Var-...          ELLIPSIS
      Var-Func         FUNC
      Var-printf       ::FUNC[ELLIPSIS][]
      Builtin-FUNC     FUNC
      Builtin-ELLIPSIS ELLIPSIS |}]

let%expect_test "types - literals" =
  show_types {|
func main ():
  let x = "3"
  let y = 3
  let z = 3.0
|};
    [%expect {|
      main     FUNC[][]
      expr-3   3
      expr-3.0 3.
      "3"      3
      Let-x    3
      Let-y    3
      Let-z    3.
      Block    ::VOID |}]

    (* open Base
 * open Coral_core
 * open Coral_frontend
 *
 * let test_source src =
 *   (match
 *      let%bind.Result imports = Utils.parse_with_imports src in
 *      let names = Coral_passes.Name_resolution.construct imports in
 *      let names = Coral_passes.Name_resolution.get_data names in
 *      let attributes, main = Coral_passes.Attribute_resolution.run imports.main in
 *      let types = Coral_types.Resolver.construct names main in
 *      Ok types
 *   with
 *   | Ok types ->
 *     Coral_types.Resolver.Resolver.dump types
 *   | Error e -> Stdio.print_endline @@ Frontend.show_parseError e)
 *
 * let test_file filename = Stdio.In_channel.read_all filename |> test_source
 *
 * let%expect_test "types - hello world" =
 *   test_source {|
 * extern("c", "printf", Func[...][])
 *
 * func foo(world):
 *   printf("Hello, %s\n", world)
 *
 * func main():
 *   let target = "world"
 *   foo target
 * |};
 *   [%expect.unreachable]
 * [@@expect.uncaught_exn {|
 *   (\* CR expect_test_collector: This test expectation appears to contain a backtrace.
 *      This is strongly discouraged as backtraces are fragile.
 *      Please change this test to not include a backtrace. *\)
 *
 *   (Failure "(\"unknown type\"(Const(Builtin FUNC)))")
 *   Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 182, characters 14-43
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 177, characters 27-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 199, characters 24-40
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 177, characters 27-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 160, characters 25-42
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 177, characters 27-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Base__List.fold_map.(fun) in file "src/list.ml", line 432, characters 23-31
 *   Called from Base__List.count_map in file "src/list.ml", line 391, characters 13-17
 *   Called from Base__List.map in file "src/list.ml" (inlined), line 418, characters 15-31
 *   Called from Base__List.fold_map in file "src/list.ml", line 431, characters 4-88
 *   Called from Coral_types__Resolver.check_types in file "src/type_resolution/resolver.ml" (inlined), line 122, characters 57-98
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 166, characters 26-45
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 160, characters 25-42
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.construct in file "src/type_resolution/resolver.ml", line 110, characters 9-112
 *   Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 11, characters 17-58
 *   Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-351
 *   Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 21, characters 2-153
 *   Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19
 *
 *   Trailing output
 *   ---------------
 *   ("resolving call" (Var (name Func) (info ())) ((Var (name ...) (info ())))) |}]
 * ;;
 * let%expect_test "types - fizzbuzz" =
 *   test_source {|
 * func fizzbuzz(n):
 *   n
 *
 * func main():
 *   fizzbuzz 20
 * |};
 *   [%expect.unreachable]
 * [@@expect.uncaught_exn {|
 *   (\* CR expect_test_collector: This test expectation appears to contain a backtrace.
 *      This is strongly discouraged as backtraces are fragile.
 *      Please change this test to not include a backtrace. *\)
 *
 *   (Failure
 *     "(\"unknown type\"(Applied(Applied(Const(Builtin FUNC))(Any))(Any)))")
 *   Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 182, characters 14-43
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 160, characters 25-42
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.construct in file "src/type_resolution/resolver.ml", line 110, characters 9-112
 *   Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 11, characters 17-58
 *   Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-351
 *   Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 38, characters 2-69
 *   Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19
 *
 *   Trailing output
 *   ---------------
 *   ("resolving call" (Var (name fizzbuzz) (info ()))
 *    ((IntLiteral (literal 20) (value 20) (info ())))) |}]
 *
 * let%expect_test "types - libs" =
 *   test_source {|
 * import raw_clib
 * import raw_posix
 *
 * func main():
 *   raw_clib.printf("Hello, %s\n", "World")
 *   raw_posix.open("/etc/passwd", 0)
 * |};
 *   [%expect.unreachable]
 * [@@expect.uncaught_exn {|
 *   (\* CR expect_test_collector: This test expectation appears to contain a backtrace.
 *      This is strongly discouraged as backtraces are fragile.
 *      Please change this test to not include a backtrace. *\)
 *
 *   (Failure "(\"unknown type\"(Const(Builtin FUNC)))")
 *   Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 182, characters 14-43
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 177, characters 27-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 199, characters 24-40
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 177, characters 27-46
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Base__List.fold_map.(fun) in file "src/list.ml", line 432, characters 23-31
 *   Called from Base__List.count_map in file "src/list.ml", line 390, characters 13-17
 *   Called from Base__List.map in file "src/list.ml" (inlined), line 418, characters 15-31
 *   Called from Base__List.fold_map in file "src/list.ml", line 431, characters 4-88
 *   Called from Coral_types__Resolver.check_types in file "src/type_resolution/resolver.ml" (inlined), line 122, characters 57-98
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 166, characters 26-45
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 160, characters 25-42
 *   Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 118, characters 25-46
 *   Called from Coral_types__Resolver.construct in file "src/type_resolution/resolver.ml", line 110, characters 9-112
 *   Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 11, characters 17-58
 *   Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-351
 *   Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 70, characters 2-143
 *   Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19
 *
 *   Trailing output
 *   ---------------
 *   ("resolving call" (Var (name Func) (info ())) ()) |}]
 *
 * let%expect_test "test - basic inference" =
 *   test_source {|
 * extern("c", "printf", Func[...][])
 *
 * # print is overloaded between int and float
 * @overload
 * func print(a:Int64):
 *   printf("integer: %ld\n", a)
 *
 * @overload
 * func print(a:Float64):
 *   printf("float: %f\n", a)
 *
 * # print2 (and the + operator is generic/ this instantiates
 * # print2$[a=Int64] and print2$[a=Float64]
 * func print2(a):
 *   print(a + a)
 *
 * func main():
 *   print2 3
 *   print2 3.1
 * |};
 *   [%expect.unreachable]
 * [@@expect.uncaught_exn {|
 *   (\* CR expect_test_collector: This test expectation appears to contain a backtrace.
 *      This is strongly discouraged as backtraces are fragile.
 *      Please change this test to not include a backtrace. *\)
 *
 *   ("[Map.add_exn] got key already present" (key print))
 *   Raised at Base__Error.raise in file "src/error.ml" (inlined), line 8, characters 14-30
 *   Called from Base__Error.raise_s in file "src/error.ml", line 9, characters 19-40
 *   Called from Base__Map.Tree0.find_and_add_or_set in file "src/map.ml", line 270, characters 10-174
 *   Called from Base__Map.Tree0.add_exn in file "src/map.ml" (inlined), line 295, characters 4-132
 *   Called from Base__Map.Accessors.add_exn in file "src/map.ml", line 1633, characters 6-175
 *   Called from Coral_passes__Name_resolution.Scope.add in file "src/name_resolution.ml" (inlined), line 15, characters 53-93
 *   Called from Coral_passes__Name_resolution.run in file "src/name_resolution.ml", line 95, characters 24-62
 *   Called from Stdlib__list.fold_left in file "list.ml", line 121, characters 24-34
 *   Called from Coral_passes__Name_resolution.run in file "src/name_resolution.ml", line 124, characters 17-53
 *   Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 8, characters 17-63
 *   Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-351
 *   Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 115, characters 2-392
 *   Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}] *)
