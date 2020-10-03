open Coral_core
open Coral_frontend

let () =
  let foo = Frontend.parse_string {| print "Hello, World!\n" |} in
  ignore foo
