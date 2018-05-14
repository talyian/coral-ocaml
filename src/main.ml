open CoralModule

external hellow : unit -> unit = "hellow"

let () =
  match Sys.argv with
  | [| _; filename |] ->   run @@ parse_file filename;
  | _ ->
     (* Tests.run Tests.main_test; *)
     run @@ parse_file "test_cases/wip.coral";
     exit 0;
  (* run @@ parse_file "test_cases/libs/pcre.coral";
   * exit 0; *)
  (* tokenize_file "test_cases/core/polymorphism_adhoc.coral"; *)
  (* Tests.run (); *)
  (* hellow (); (\* test c bindings work *\) *)
  (* run @@ parse_file "test_cases/core/hello.coral";
   * run @@ parse_file "test_cases/core/fizzbuzz.coral";
   * run @@ parse_file "test_cases/core/factorial.coral";
   * run @@ parse_file "test_cases/core/collatz.coral";
   * run @@ parse_file "test_cases/core/inference.coral";
   * run @@ parse_file "test_cases/core/polymorphism_adhoc.coral";
   * run @@ parse_file "test_cases/core/tuples.coral";
   * run @@ parse_file "test_cases/core/def.coral"; *)
  (* Ast.show @@ parse_file "test_cases/core/polymorphism_parametric.coral"; *)
