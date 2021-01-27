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
     let%map.Result imports = Utils.parse_with_imports src in
     Coral_passes.Name_resolution.construct imports
  with
  | Ok names -> Coral_passes.Name_resolution.show names
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e);
  [%expect {|
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
        [Var-world] -> Param-world |}]
