open OUnit2

(* the main entry point for tests *)
let run test = test
  |> OUnit2.run_test_tt_main
  |> ignore

let parse_test file ctxt =
  CoralModule.run @@ CoralModule.parse_file file

(* Forks and asserts that the child process writes a known string to stdout *)
let output_test expected_output file ctxt =
  (* this is required since any previous code may have unflushed output
     that will get written after we dup2 onto the output pipe *)
  flush stdout;
  let input, output = Unix.pipe() in
  let run_child () =
     Unix.close input;
     Unix.dup2 output Unix.stdout;
     CoralModule.run @@ CoralModule.parse_file file;
     Unix.close output;
     exit 0 in
  let run_parent child =
     Unix.close output;
     let child_output =
       let rec final_output () =
         match Unix.waitpid [ ] child with
         | (pid, Unix.WEXITED n) -> n
         | _ -> final_output () in
       let len = 102400 in
       let buf = Bytes.create len in
       let read = Unix.read input buf 0 len in
       Unix.close input;
       Bytes.sub_string buf 0 read in
     OUnit2.assert_equal ~printer:(fun i -> i) expected_output child_output in
  match Unix.fork () with
  | 0 -> run_child()
  | pid -> run_parent pid

let main_test = "Tests" >::: [
  "hello" >::
    output_test "Hello, World!\n" "test_cases/core/hello.coral";
  "factorial" >::
    (let expected =
       [6,720; 8,40320; 14,87178291200]
       |> List.map (fun (i,f) -> Printf.sprintf "%5d! = %d\n" i f)
       |> String.concat "" in
     output_test expected "test_cases/core/factorial.coral");
  "fizzbuzz" >:: output_test
    "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz\n\n"
    "test_cases/core/fizzbuzz.coral";
   "collatz" >:: output_test
     "1 7 2 5 8 16 3 19 6 14 9 9 17 17 4 12 20 20 7 7 15 15 10 23 10 111 18 18 \n"
     "test_cases/core/collatz.coral";
 "inference" >:: parse_test "test_cases/core/inference.coral";
 "adhoc-polymorph" >:: parse_test "test_cases/core/polymorphism_adhoc.coral";
 "tuples" >:: parse_test "test_cases/core/tuples.coral";
 "def-uniqueness" >:: output_test "def test: 4 6.5" "test_cases/core/def.coral";
]


let () = run main_test
