open CoralModule

external hellow : unit -> unit = "hellow"

let () =
  Tests.run Tests.main_test;
  (* Tests.run_def_test(); *)
  (* run @@ parse_file "test_cases/core/def.coral";
   * exit 0; *)
  (* run @@ parse_file "test_cases/libs/pcre.coral";
   * exit 0; *)
  (* CoralModule.tokenize_file "test_cases/core/polymorphism_adhoc.coral"; *)
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
