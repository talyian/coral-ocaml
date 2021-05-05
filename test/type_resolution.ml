open Base
open Coral_core
open Coral_frontend

let get_types source =
  let%bind.Result imports = Test_utils.parse_with_imports source in
    let names = Coral_passes.Name_resolution.construct imports in
    (* Coral_passes.Name_resolution.show names; *)
    let names = Coral_passes.Name_resolution.get_data names in
(* TODO: running attribute resolution after name resolution has the problem that
any new nodes we generate have to be remapped in the name reference table.
   It would really like to go after name resolution, however, since both the
   decorator name and its arguments can depend on name resolution *)
  (* let attributes, main = Coral_passes.Attribute_resolution.run imports.main in *)
    let types = Coral_types.Resolver.construct names imports.main in
  Ok types

let run_test_file file =
  match
  let%map.Result imports = Test_utils.parse_file_with_imports file in
  let names = Coral_passes.Name_resolution.construct imports in
  let names = Coral_passes.Name_resolution.get_data names in
  let types = Coral_types.Resolver.construct names imports.main in
  types
  with | Ok types ->
    Coral_types.Resolver.dump types;
  | Error e ->
    Stdio.print_endline @@ Frontend.show_parseError e;
  | exception e ->
    Stdio.print_endline @@ Exn.to_string e

let show_types source = match get_types source
with
  | Ok types ->
    Coral_types.Resolver.dump types;
  | Error e ->
    Stdio.print_endline @@ Frontend.show_parseError e;
  | exception e ->
    Stdio.print_endline @@ Exn.to_string e
;;

let show_line types line =
  match types with
  | Ok (types:Coral_types.Resolver.Resolver.t) ->
    Map.filter_keys ~f:(fun k -> String.(Ast.show_short k = line))  types.types
    |> Map.min_elt_exn |> snd |> Coral_types.Resolver.TypeSpec.show
    |> Stdio.print_endline
  | _ -> ()

let%expect_test "types - hello world" =
  show_types {|
extern("c", "printf", Func[...][])

func main ():
  printf("Hello, %s\n", "World")
|};
    [%expect {|
      (var-check-type
       ((name Func) (reference (Builtin (builtin FUNC))) (typ FUNC)))
      (var-check-type
       ((name ...) (reference (Builtin (builtin ELLIPSIS))) (typ ELLIPSIS)))
      (extern
       ((name printf)
        (typ
         (Call (callee (Call (callee (Var Func)) (args ((Var ...))))) (args ())))
        (typ_type FUNC)))
      (var-check-type
       ((name printf)
        (reference
         (Extern (binding c) (name printf)
          (typ
           (Call (callee (Call (callee (Var Func)) (args ((Var ...))))) (args ())))))
        (typ (instance_of FUNC))))
      (Failure
        "(\"unknown instantiation\"((callee(Var printf))(callee_type(instance_of FUNC))(args_types(\"Hello, %s\\n\"World))))") |}]

let%expect_test "types - literals" =
  let types = get_types {|
func main ():
  let x = "3"
  let y = 3
  let z = 3.0
  let typeof_triple = typeof(x, y, z)
  let triple_oftype = (typeof x, typeof y, typeof z)
|} in
  ignore [%expect.output];
  show_line types "Let-typeof_triple";
  [%expect{| TUPLE['3', 3i, 3.f] |}];
  show_line types "Let-triple_oftype";
  [%expect{| {(STR INT64 FLOAT64)} |}]

let%expect_test "types - imports" =
  show_types {|
import raw_clib

func main():
  let format = raw_clib.malloc 10
  raw_clib.memcpy(format, "Hello", 5)
  raw_clib.memcpy(format + 5, ", %g", 5)
  raw_clib.printf(format, 3.1416)
|}

;[%expect {|
  (var-check-type
   ((name Func) (reference (Builtin (builtin FUNC))) (typ FUNC)))
  (var-check-type
   ((name Uint64) (reference (Builtin (builtin UINT64))) (typ UINT64)))
  (var-check-type ((name Ptr) (reference (Builtin (builtin PTR))) (typ PTR)))
  (var-check-type
   ((name Uint8) (reference (Builtin (builtin UINT8))) (typ UINT8)))
  (var-check-type
   ((name Cstr)
    (reference
     (Let (name Cstr) (typ ())
      (value (Index (callee (Var Ptr)) (args ((Var Uint8)))))))
    (typ PTR)))
  (extern
   ((name malloc)
    (typ
     (Call (callee (Call (callee (Var Func)) (args ((Var Uint64)))))
      (args ((Var Cstr)))))
    (typ_type FUNC)))
  (Failure
    "(\"unknown instantiation\"((callee(Member(base(Var raw_clib))(member malloc)))(callee_type(instance_of FUNC))(args_types(10))))") |}]


let%expect_test "type-resolution -- regex-redux" =
    run_test_file "examples/benchmarks_game/regex-redux.coral";
    [%expect {|
    "Member reference not found: Member {base = (Var stdin); member = \"fd\"}" |}]


(* TODO: this has an issue because I guess Ast.fold_map is creating new refs
   when we need to keep the old refs *)
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
