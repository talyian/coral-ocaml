open CoralModule

external hellow : unit -> unit = "hellow"

let () =
  (* CoralModule.tokenize_file "samples/core/polymorphism_adhoc.coral"; *)
  (* Tests.run (); *)
  run @@ parse_file "samples/core/hello.coral";
  run @@ parse_file "samples/core/fizzbuzz.coral";
  run @@ parse_file "samples/core/factorial.coral";
  run @@ parse_file "samples/core/collatz.coral";
  run @@ parse_file "samples/core/tuples.coral";
  run @@ parse_file "samples/core/polymorphism_adhoc.coral";
  (* run @@ parse_file "samples/core/polymorphism_parametric.coral"; *)
