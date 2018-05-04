open OUnit

external hellow : unit -> unit = "hellow"

let parse_test file x =
  CoralModule.run @@ CoralModule.parse_file file

let run () =
  "Tests" >::: [
      "hello" >:: parse_test "samples/core/hello.coral";
      "fizzbuzz" >:: parse_test "samples/core/fizzbuzz.coral";
      "factorial" >:: parse_test "samples/core/factorial.coral";
      "collatz" >:: parse_test "samples/core/collatz.coral";
    ]
  |> OUnit.run_test_tt ~verbose:false
  |> ignore
