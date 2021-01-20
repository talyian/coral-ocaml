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
     let expr = imports.main in
     Coral_passes.Name_resolution.resolve expr
  with
  | Ok names -> Coral_passes.Name_resolution.show names
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e);
  [%expect {|
    Names
        [ref (Var {name = "foo"; info = ()})] -> ref (Func {name = "foo"; ret_type = None;
           params =
           [ref (Param {idx = 0; name = "world"; typ = None; info = ()})];
           body =
           ref (Call {callee = ref (Var {name = "printf"; info = ()});
                  args =
                  [ref (StringLiteral {literal = "Hello, %s\n"; info = ()});
                    ref (Var {name = "world"; info = ()})];
                  info = ()});
           info = ()})
        [ref (Var {name = "printf"; info = ()})] -> ref (Var {name = "printf"; info = ()})
        [ref (Var {name = "target"; info = ()})] -> ref (Let {name = "target"; typ = None;
           value = ref (StringLiteral {literal = "world"; info = ()}); info = ()})
        [ref (Var {name = "world"; info = ()})] -> ref (Param {idx = 0; name = "world"; typ = None; info = ()}) |}]
