open Base
open Coral_core
open Coral_frontend

let test_source src =
  (match
     let%bind.Result imports = Utils.parse_with_imports src in
     let names = Coral_passes.Name_resolution.construct imports in
     let names = Coral_passes.Name_resolution.get_data names in
     let attributes, main = Coral_passes.Attribute_resolution.run imports.main in
     let types = Coral_types.Resolver.construct names main in
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
|}]
;;
let%expect_test "types - fizzbuzz" =
  test_source {|
func fizzbuzz(n):
  n

func main():
  fizzbuzz 20
|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "instantiate1: oops")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Coral_types__Resolver.instantiate in file "src/type_resolution/resolver.ml", line 216, characters 28-72
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 170, characters 26-69
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 117, characters 25-46
  Called from Coral_types__Resolver.check_type_raw in file "src/type_resolution/resolver.ml", line 151, characters 25-42
  Called from Coral_types__Resolver.check_type in file "src/type_resolution/resolver.ml", line 117, characters 25-46
  Called from Coral_types__Resolver.construct in file "src/type_resolution/resolver.ml", line 109, characters 9-112
  Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 11, characters 17-58
  Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-351
  Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 91, characters 2-69
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]

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
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]

let%expect_test "test - basic inference" =
  test_source {|
extern("c", "printf", Func[...][])

# print is overloaded between int and float
@overload
func print(a:Int64):
  printf("integer: %ld\n", a)

@overload
func print(a:Float64):
  printf("float: %f\n", a)

# print2 (and the + operator is generic/ this instantiates
# print2$[a=Int64] and print2$[a=Float64]
func print2(a):
  print(a + a)

func main():
  print2 3
  print2 3.1
|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("[Map.add_exn] got key already present" (key print))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 8, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 9, characters 19-40
  Called from Base__Map.Tree0.find_and_add_or_set in file "src/map.ml", line 270, characters 10-174
  Called from Base__Map.Tree0.add_exn in file "src/map.ml" (inlined), line 295, characters 4-132
  Called from Base__Map.Accessors.add_exn in file "src/map.ml", line 1633, characters 6-175
  Called from Coral_passes__Name_resolution.Scope.add in file "src/name_resolution.ml" (inlined), line 15, characters 53-93
  Called from Coral_passes__Name_resolution.run in file "src/name_resolution.ml", line 95, characters 24-62
  Called from Stdlib__list.fold_left in file "list.ml", line 121, characters 24-34
  Called from Coral_passes__Name_resolution.run in file "src/name_resolution.ml", line 124, characters 17-53
  Called from Tests__Type_resolution.test_source.(fun) in file "test/type_resolution.ml", line 8, characters 17-63
  Called from Tests__Type_resolution.test_source in file "test/type_resolution.ml", line 7, characters 5-351
  Called from Tests__Type_resolution.(fun) in file "test/type_resolution.ml", line 194, characters 2-392
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
