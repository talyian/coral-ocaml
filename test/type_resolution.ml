open Base
open Coral_core
open Coral_frontend

let%expect_test "types" =
  let src = {|
import raw_clib (printf)

func foo(world):
  printf("Hello, %s\n", world)

func main():
  let target = "world"
  foo target
|} in
  (match
     let%bind.Result imports = Utils.parse_with_imports src in
     let names = Coral_passes.Name_resolution.(get_data @@ construct imports) in
     let types = Coral_types.Resolver.construct names imports.main in
     Ok types
  with
  | Ok types ->
    Coral_types.Resolver.Resolver.dump types
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e);
  [%expect {|
    Call-Call-Var-Func: (Resolver.TypeSpec.Const FUNC)
    Call-Var-Func: (Resolver.TypeSpec.Const FUNC)
    Call-Var-Ptr: (Resolver.TypeSpec.Const PTR)
    Call-Var-foo: (Resolver.TypeSpec.Const FUNC)
    Call-Var-printf: (Resolver.TypeSpec.Const FUNC)
    Extern-printf: (Resolver.TypeSpec.Const FUNC)
    Func-foo: (Resolver.TypeSpec.Const FUNC)
    Func-main: (Resolver.TypeSpec.Const VOID)
    expr: Resolver.TypeSpec.Any
    "Hello, %s
    ": (Resolver.TypeSpec.Const STR)
    "world": (Resolver.TypeSpec.Const STR)
    Var-...: (Resolver.TypeSpec.Const ELLIPSIS)
    Var-Func: (Resolver.TypeSpec.Const FUNC)
    Var-Ptr: (Resolver.TypeSpec.Const PTR)
    Var-Uint8: (Resolver.TypeSpec.Const UINT8)
    Var-foo: (Resolver.TypeSpec.Const FUNC)
    Var-printf: (Resolver.TypeSpec.Const FUNC)
    Var-target: (Resolver.TypeSpec.Const STR)
    Var-world: Resolver.TypeSpec.Any
    Let-target: (Resolver.TypeSpec.Const STR)
    Block: (Resolver.TypeSpec.Const VOID)
    Builtin-UINT8: (Resolver.TypeSpec.Const UINT8)
    Builtin-PTR: (Resolver.TypeSpec.Const PTR)
    Builtin-FUNC: (Resolver.TypeSpec.Const FUNC)
    Builtin-ELLIPSIS: (Resolver.TypeSpec.Const ELLIPSIS) |}]
