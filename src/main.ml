open CoralModule

external hellow : unit -> unit = "hellow"

let () =
  run @@ parse_file "samples/core/tuples.coral"
  (* Tests.run () *)
  (* run @@ parse_file "samples/core/hello.coral";
   * run @@ parse_file "samples/core/fizzbuzz.coral";
   * run @@ parse_file "samples/core/factorial.coral";
   * run @@ parse_file "samples/core/collatz.coral"; *)
