open Base
open Coral_core
open Coral_frontend

let%expect_test "names" =
  let src = {|
import raw_clib (printf)

extern("c", "printf", Func[][Ptr[Uint8], ...])

func foo(world):
  printf("Hello, %s\n", world)

func main():
  let target = "world"
  foo target
|} in
  (match
     let%map.Result imports = Utils.parse_with_imports src in
     Coral_passes.Name_resolution.resolve imports
  with
  | Ok names -> Coral_passes.Name_resolution.show names
  | Error e -> Stdio.print_endline @@ Frontend.show_parseError e);
  [%expect {|
    Names
        [ref (Var {name = "..."; info = ()})] -> ref (Builtin {builtin = ELLIPSIS; info = ()})
        [ref (Var {name = "Func"; info = ()})] -> ref (Builtin {builtin = FUNC; info = ()})
        [ref (Var {name = "Ptr"; info = ()})] -> ref (Builtin {builtin = PTR; info = ()})
        [ref (Var {name = "Uint8"; info = ()})] -> ref (Builtin {builtin = UINT8; info = ()})
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
        [ref (Var {name = "printf"; info = ()})] -> ref (Extern {binding = "c"; name = "printf";
           typ =
           ref (Call {
                  callee =
                  ref (Call {callee = ref (Var {name = "Func"; info = ()});
                         args = []; info = ()});
                  args =
                  [ref (Call {callee = ref (Var {name = "Ptr"; info = ()});
                          args = [ref (Var {name = "Uint8"; info = ()})];
                          info = ()});
                    ref (Var {name = "..."; info = ()})];
                  info = ()});
           info = ()})
        [ref (Var {name = "target"; info = ()})] -> ref (Let {name = "target"; typ = None;
           value = ref (StringLiteral {literal = "world"; info = ()}); info = ()})
        [ref (Var {name = "world"; info = ()})] -> ref (Param {idx = 0; name = "world"; typ = None; info = ()}) |}]
