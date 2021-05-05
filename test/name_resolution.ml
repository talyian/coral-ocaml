open Base
open Coral_core
open Coral_frontend

let%expect_test "names" =
  let src = {|
import raw_clib (printf)

func foo(world):
  printf("Hello, %s\n", world)

func main():
  let target = "world"
  foo target
|} in
  (match
     let%map.Result imports = Test_utils.parse_with_imports src in
     Coral_passes.Name_resolution.construct imports
  with
  | Ok names -> Coral_passes.Name_resolution.show names
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e);
  [%expect{|
    Names
        [Var-...] -> Builtin-ELLIPSIS
        [Var-Cstr] -> Let-Cstr
        [Var-Func] -> Builtin-FUNC
        [Var-Int32] -> Builtin-INT32
        [Var-Int64] -> Builtin-INT64
        [Var-Ptr] -> Builtin-PTR
        [Var-Uint64] -> Builtin-UINT64
        [Var-Uint8] -> Builtin-UINT8
        [Var-foo] -> foo
        [Var-printf] -> Extern-printf
        [Var-target] -> Let-target
        [Var-world] -> Param-world
    Names.members
        [<module>.foo] -> foo
        [<module>.main] -> main
        [<module>.printf] -> Extern-printf
        [raw_clib.Cstr] -> Let-Cstr
        [raw_clib.free] -> Extern-free
        [raw_clib.malloc] -> Extern-malloc
        [raw_clib.memcpy] -> Extern-memcpy
        [raw_clib.memmove] -> Extern-memmove
        [raw_clib.memset] -> Extern-memset
        [raw_clib.printf] -> Extern-printf |}]

let run_test_file filename =
  (match
    let%map.Result imports = Test_utils.parse_file_with_imports filename in
    Coral_passes.Name_resolution.construct imports
  with
  | Ok names ->
    Coral_passes.Name_resolution.show ~only_names:() names;
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e;
  | exception e ->
    (Backtrace.elide := false;
    Stdio.print_endline @@ Exn.to_string e;
     Stdio.print_endline @@ Backtrace.to_string @@ Backtrace.Exn.most_recent ()))

let%expect_test "name-resolution -- binary-trees" =
    run_test_file "examples/benchmarks_game/binary-trees.coral";
    [%expect {|
    Names
        [Var-+] -> Overload-Overload
        [Var--] -> Overload-Overload
        [Var-...] -> Builtin-ELLIPSIS
        [Var-=] -> Overload-Overload
        [Var-??] -> Builtin-(Custom "??")
        [Var-Cstr] -> Let-Cstr
        [Var-FdReader] -> TypeDecl-FdReader
        [Var-FdWriter] -> TypeDecl-FdWriter
        [Var-Func] -> Builtin-FUNC
        [Var-Int32] -> Builtin-INT32
        [Var-Int64] -> Builtin-INT64
        [Var-Ptr] -> Builtin-PTR
        [Var-Tree] -> TypeDecl-Tree
        [Var-Uint64] -> Builtin-UINT64
        [Var-Uint8] -> Builtin-UINT8
        [Var-check] -> check
        [Var-console] -> console
        [Var-depth] -> Param-depth
        [Var-i32] -> Builtin-(Custom "i32")
        [Var-max] -> Builtin-(Custom "max")
        [Var-max_depth] -> Let-max_depth
        [Var-max_tree] -> Let-max_tree
        [Var-null] -> Builtin-(Custom "NULL")
        [Var-printf] -> Extern-printf
        [Var-raw_clib] -> raw_clib
        [Var-t] -> Param-t
        [Var-tree] -> tree |}]

let%expect_test "name-resolution -- pidigits" =
    run_test_file "examples/benchmarks_game/pidigits.coral";
    [%expect {|
    Names
        [Var-*] -> Overload-Overload
        [Var-+] -> Overload-Overload
        [Var--] -> Overload-Overload
        [Var-/] -> Overload-Overload
        [Var-=] -> Overload-Overload
        [Var-Int64] -> Builtin-INT64
        [Var-bigint] -> TypeDecl-bigint
        [Var-console] -> console
        [Var-d] -> Let-d
        [Var-digits] -> digits
        [Var-k] -> Let-k
        [Var-n1] -> Let-n1
        [Var-n2] -> Let-n2
        [Var-u] -> Let-u
        [Var-v] -> Let-v
        [Var-w] -> Let-w |}]

let%expect_test "name-resolution -- regex-redux" =
    run_test_file "examples/benchmarks_game/regex-redux.coral";
    [%expect {|
    Names
        [Var-...] -> Builtin-ELLIPSIS
        [Var-FdReader] -> TypeDecl-FdReader
        [Var-FdWriter] -> TypeDecl-FdWriter
        [Var-Func] -> Builtin-FUNC
        [Var-Regex] -> TypeDecl-Regex
        [Var-Str] -> Builtin-STR
        [Var-printf] -> Extern-printf
        [Var-regex] -> regex
        [Var-seq] -> Let-seq
        [Var-stdin] -> Let-stdin |}]
