open CoralModule

external hellow : unit -> unit = "hellow"

let () =
  run @@ parse_file "samples/libs/pcre.coral";
  exit 0;
  (* CoralModule.tokenize_file "samples/core/polymorphism_adhoc.coral"; *)
  (* Tests.run (); *)
  hellow (); (* test c bindings work *)
  run @@ parse_file "samples/core/hello.coral";
  run @@ parse_file "samples/core/fizzbuzz.coral";
  run @@ parse_file "samples/core/factorial.coral";
  run @@ parse_file "samples/core/collatz.coral";
  run @@ parse_file "samples/core/tuples.coral";
  run @@ parse_file "samples/core/inference.coral";
  run @@ parse_file "samples/core/polymorphism_adhoc.coral";
  (* Ast.show @@ parse_file "samples/core/polymorphism_parametric.coral"; *)
