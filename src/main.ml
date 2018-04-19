open CoralModule
let tee f x = f x; x
let () =
  "samples/core/hello.coral"  
  |> parse_file
  |> run
  (* let parse_test filename = parse_file filename |> ignore in
   * let suite = "Tests" >::: [
   *   "Core" >::: [
   *     (\* basics *\)
   *     "hello_world" >:: (fun _ -> parse_test "samples/core/hello.coral");
   *     (\* "factorial" >:: (fun _ -> parse_test "samples/core/factorial.coral");
   *      * "collatz"  >:: (fun _ -> parse_test "samples/core/collatz.coral"); *\)
   *     (\* inference *\)
   *     (\* "inference" >:: (fun _ -> parse_test "samples/core/inference.coral"); *\)
   *     (\* "generics" >:: (fun _ -> parse_test "samples/core/tuples.coral");
   *      * "polymorphism" >:: (fun _ -> parse_test "samples/core/polymorphism_adhoc.coral"); *\)
   *   ]
   * ]
   * in
   * OUnit.run_test_tt suite |> ignore
   * (\* exit 0;
   *  * (\\* let f = open_in "samples/polymorphism_adhoc.coral" in *\\)
   *  * let f = open_in "samples/collatz.coral" in
   *  * (\\* let f = open_in "samples/factorial.coral" in *\\)
   *  * let lexbuf = Lexing.from_channel f in
   *  * parse lexbuf |> run *\) *)
