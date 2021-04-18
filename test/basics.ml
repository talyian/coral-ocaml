open Base

let%expect_test "parse-hello-world" =
  let _ =
    let%map.Result imports = Test_utils.parse_with_imports {|
import raw_clib
raw_clib.printf("Hello, %s\n", "World")|} in
    Test_utils.dump_expr imports.main in
  [%expect {|
    Module {name = "";
      lines =
      [Import {path = ["raw_clib"]; names = [(Ast.Module None)]};
        Call {callee = Member {base = (Var raw_clib); member = "printf"};
          args =
          [StringLiteral {literal = "Hello, %s\n"};
            StringLiteral {literal = "World"}]}
        ]} |}]


let%expect_test "import-clib" =
  (match Test_utils.parse_with_imports {|
import cstd

cstd.printf("Hello, %s\n", "World")|} with
  | Ok imports -> Test_utils.dump_expr imports.main
  | Error e -> Stdio.print_endline @@ Coral_frontend.Frontend.show_parseError e);
  [%expect {|
    Module {name = "";
      lines =
      [Import {path = ["cstd"]; names = [(Ast.Module None)]};
        Call {callee = Member {base = (Var cstd); member = "printf"};
          args =
          [StringLiteral {literal = "Hello, %s\n"};
            StringLiteral {literal = "World"}]}
        ]} |}]


let%expect_test "test-raw_posix" =
  (match Test_utils.parse_with_imports {|
import raw_posix
import raw_clib (malloc, free, printf)

func main():
  let passwd = raw_posix.open("/etc/passwd", 0)
  let len = 1024
  let buf = malloc len
  let len = raw_posix.read(passwd, buf, len)
  raw_posix.close(passwd)
  printf("passwd:[%.*s]\\n", len, buf)
  free buf
|} with
  | Ok _ -> ()
  | Error e -> Stdio.print_endline @@ Test_utils.clean_vt_escape @@ Coral_frontend.Frontend.show_parseError e);
  [%expect {| |}]
