open Base

module Lexer = struct
  module Regex = struct
    type t =
      | Char of char
      | Seq of t * t
      | Or of t * t
      | Star of t
      | Plus of t
      | Range of char * char
      | Any
      | Empty
    [@@deriving sexp]

    type deriv = (t, unit) Result.t [@@deriving sexp]

    let rec nullable = function
      | Char _ -> false
      | Range _ -> false
      | Any -> false
      | Seq (a, b) -> nullable a && nullable b
      | Or (a, b) -> nullable a || nullable b
      | Star _ -> true
      | Plus x -> nullable x
      | Empty -> true

    let rec deriv c (expr : t) : deriv =
      match expr with
      | Empty -> Error ()
      | Char x when Char.equal x c -> Ok Empty
      | Char _ -> Error ()
      | Range (a, b) -> if Char.(a <= c && c <= b) then Ok Empty else Error ()
      | Any -> Ok Empty
      | Star x -> (
        match deriv c x with Error () -> Error () | Ok dx -> Ok (Seq (dx, x)) )
      | Plus x -> deriv c (Seq (x, Star x))
      | Or (a, b) -> (
        match (deriv c a, deriv c b) with
        | Error (), a -> a
        | a, Error () -> a
        | Ok a, Ok b -> Ok (Or (a, b)) )
      | Seq (a, b) -> (
        match (deriv c a, nullable a) with
        | Error (), true -> deriv c b
        | Error (), false -> Error ()
        | Ok da, false -> Ok (Seq (da, b))
        | Ok da, true -> (
          match deriv c b with
          | Error () -> Ok (Seq (da, b))
          | Ok db -> Ok (Or (Seq (da, b), Seq (da, db))) ) )

    let rec matches (target : string) (last_succ : int option) (index : int)
        (pattern : t) =
      match deriv (String.get target index) pattern with
      | Ok Empty -> Ok (index + 1)
      | Ok d ->
          let last_succ = if nullable d then Some (index + 1) else last_succ in
          matches target last_succ (index + 1) d
      | Error () | (exception _) -> (
        match last_succ with Some i -> Ok i | None -> Error "end of string" )

    let rec show level = function
      | Any -> "."
      | Char (('.' | '*' | '?' | '(' | ')' | '[' | ']' | '+' | '{' | '}') as c)
        ->
          "\\" ^ String.make 1 c
      | Char c -> String.make 1 c
      | Seq (a, b) ->
          (if level > 0 then "(" else "")
          ^ show 0 a ^ show 0 b
          ^ if level > 0 then ")" else ""
      | Or (a, b) -> show 0 a ^ "|" ^ show 0 b
      | Star s -> show 1 s ^ "*"
      | Range (a, b) -> "[" ^ String.make 1 a ^ "-" ^ String.make 1 b ^ "]"
      | t -> Sexp.to_string @@ sexp_of_t t

    let show = show 0
    let show_deriv = function Ok s -> show s | Error e -> "error"

    let%expect_test "test pattern matches" =
      List.iter ~f:Stdio.print_s
        (let%bind.List target = ["banana"; "apple"; "ugli fruit"] in
         let%bind.List pattern =
           [ Seq (Any, Star (Seq (Char 'a', Char 'n')))
           ; Seq (Range ('o', 'z'), Plus (Range ('a', 'z'))) ] in
         let%map.List result =
           match matches target None 0 pattern with
           | Ok index -> [String.sub target ~pos:0 ~len:index]
           | _ -> [] in
         [%sexp (target : string), (show pattern : string), (result : string)]
        ) ;
      [%expect
        {|
        (banana ".(an)*" banan)
        (apple ".(an)*" a)
        ("ugli fruit" ".(an)*" u)
        ("ugli fruit" "[o-z](Plus(Range a z))" ugli) |}]
  end

  module Grammar = struct
    type action = string
    type rule = Regex.t * action
    type grammar = rule list

    let create () : grammar = []
    let add_rule target action (g : grammar) = (target, action) :: g

    let%expect_test "arithmetic grammar" =
      let rec seqs = function
        | [a] -> a
        | x :: a -> Regex.Seq (x, seqs a)
        | [] -> failwith "nonempty list required" in
      let numd = Regex.Range ('0', '9') in
      let grammar =
        create ()
        |> add_rule Regex.(Plus (Char ' ')) "whitespace"
        |> add_rule Regex.(Plus (Range ('0', '9'))) "integer"
        |> add_rule
             Regex.(seqs [Star numd; Char '.'; Plus (Range ('0', '9'))])
             "float"
        |> add_rule Regex.(Plus (Range ('a', 'z'))) "id" in
      ()
  end

  module Interpreter = struct end
  module Compiler = struct end
end
