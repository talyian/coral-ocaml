open Base

(* A lexer is a machine that takes in a stream of input tokens and produces a
       stream of output tokens, using some internal state.

       Performance: Because we're recognizing regular grammars, we can
       implement a lexer that doesn't need runtime allocation.

       Composition: The input and output are both token streams, so we can chain lexers together.
   This is useful for chaining unicode lexers that turn byte streams into character streams
   and our custom lexers that operate on characters.
*)

(** Represents the input to a lexer. element is the token type and collection is
an abstraction over a fixed length collection of tokens. TODO: shouldn't
collection be an iterator? *)
module type Input = sig
  type element

  val sexp_of_element : element -> Sexp.t
  val equal : element -> element -> bool
  val ( <= ) : element -> element -> bool

  type collection

  val length : collection -> int
  val get : collection -> int -> element
end

module Regex_intf (C : sig
  type t [@@deriving sexp_of]
end) =
struct
  type t =
    | Empty
    | Char of C.t
    | Range of C.t * C.t
    | Any
    | Star of t
    | Plus of t
    | Seq of t * t
    | Or of t * t
  [@@deriving sexp_of]
end

module type Regex = sig
  type c [@@deriving sexp_of]
  type input_collection

  include module type of Regex_intf (struct type t = c [@@deriving sexp_of] end)

  val collection_length : input_collection -> int
  val derive : c -> t -> (t, unit) Result.t
  val matches : input_collection -> int option -> int -> t -> int Or_error.t
end

module Regex = struct
  module type S = Regex

  module Make (Input : Input) = struct
    type c = Input.element [@@deriving sexp_of]
    type input_collection = Input.collection

    let collection_length = Input.length

    include Regex_intf (struct type t = Input.element [@@deriving sexp_of] end)

    type derivative = (t, unit) Result.t

    let rec nullable = function
      | Char _ -> false
      | Range _ -> false
      | Any -> false
      | Seq (a, b) -> nullable a && nullable b
      | Or (a, b) -> nullable a || nullable b
      | Star _ -> true
      | Plus x -> nullable x
      | Empty -> true

    let rec derive c (expr : t) : derivative =
      match expr with
      | Empty -> Error ()
      | Char x when Input.equal x c -> Ok Empty
      | Char _ -> Error ()
      | Range (a, b) -> if Input.(a <= c && c <= b) then Ok Empty else Error ()
      | Any -> Ok Empty
      | Star x -> (
        match derive c x with Error () -> Error () | Ok dx -> Ok (Seq (dx, x)) )
      | Plus x -> derive c (Seq (x, Star x))
      | Or (a, b) -> (
        match (derive c a, derive c b) with
        | Error (), a -> a
        | a, Error () -> a
        | Ok a, Ok b -> Ok (Or (a, b)) )
      | Seq (a, b) -> (
        match (derive c a, nullable a) with
        | Error (), true -> derive c b
        | Error (), false -> Error ()
        | Ok da, false -> Ok (Seq (da, b))
        | Ok da, true -> (
          match derive c b with
          | Error () -> Ok (Seq (da, b))
          | Ok db -> Ok (Or (Seq (da, b), Seq (da, db))) ) )

    let rec matches (target : Input.collection) (last_succ : int option)
        (index : int) (pattern : t) =
      match derive (Input.get target index) pattern with
      | Ok Empty -> Ok (index + 1)
      | Ok d ->
          let last_succ = if nullable d then Some (index + 1) else last_succ in
          matches target last_succ (index + 1) d
      | Error () | (exception _) -> (
        match last_succ with
        | Some i -> Ok i
        | None -> Or_error.error_string "end of string" )
  end
end

module type Grammar = sig
  module Input : Input
  module Regex : Regex

  type action
  type regex = Regex.t
  type t

  val create : unit -> t
  val add_rule : regex -> action -> t -> t
  val get_rules : t -> (regex * action) list
end

module Grammar_Make
    (Input : Input)
    (Regex : Regex) (Output : sig
      type t
      (* type collection *)

      (* val append : t -> collection -> collection *)
    end) =
struct
  module Input = Input
  module Regex = Regex

  type action = Output.t
  type regex = Regex.t
  type rule = Regex.t * action
  type t = {rules: rule list}

  let create () = {rules= []}
  let add_rule target action {rules= g} = {rules= (target, action) :: g}
  let get_rules {rules} = rules

  let deriv c {rules= g} =
    let rules =
      List.filter_map g ~f:(fun (regex, action) ->
          match Regex.derive c regex with
          | Ok d -> Some (d, action)
          | Error _ -> None ) in
    {rules}
end

module Interpreter (Grammar : Grammar) = struct
  type tokens = (int * int * Grammar.action) list

  let rec recognize grammar start source acc : (tokens, string) Result.t =
    let token =
      List.find_map (Grammar.get_rules grammar) ~f:(fun (pattern, action) ->
          match Grammar.Regex.matches source None start pattern with
          | Ok i -> Some (start, i, action)
          | Error _ -> None ) in
    match token with
    | None -> Error (Printf.sprintf "Syntax error at %d\n" start)
    | Some (start, index, action) ->
        let new_token = (start, index, action) in
        let acc = new_token :: acc in
        if index >= Grammar.Regex.collection_length source then
          Ok (List.rev acc)
        else recognize grammar index source acc
end

let%expect_test "test pattern matches" =
  let open Regex.Make (struct
    type element = char [@@deriving sexp_of]
    type collection = string

    let equal = Char.equal
    let ( <= ) = Char.( <= )
    let length = String.length
    let get = String.get
  end) in
  let rec show level = function
    | Any -> "."
    | Char (('.' | '*' | '?' | '(' | ')' | '[' | ']' | '+' | '{' | '}') as c) ->
        "\\" ^ String.make 1 c
    | Char c -> String.make 1 c
    | Seq (a, b) ->
        (if level > 0 then "(" else "")
        ^ show 0 a ^ show 0 b
        ^ if level > 0 then ")" else ""
    | Or (a, b) -> show 0 a ^ "|" ^ show 0 b
    | Star s -> show 1 s ^ "*"
    | Range (a, b) -> "[" ^ String.make 1 a ^ "-" ^ String.make 1 b ^ "]"
    | t -> Sexp.to_string @@ sexp_of_t t in
  let show = show 0 in
  List.iter ~f:Stdio.print_s
    (let%bind.List target = ["banana"; "apple"; "ugli fruit"] in
     let%bind.List pattern =
       [ Seq (Any, Star (Seq (Char 'a', Char 'n')))
       ; Seq (Range ('o', 'z'), Plus (Range ('a', 'z'))) ] in
     let%map.List result =
       match matches target None 0 pattern with
       | Ok index -> [String.sub target ~pos:0 ~len:index]
       | _ -> [] in
     [%sexp (target : string), (show pattern : string), (result : string)] ) ;
  [%expect
    {|
    (banana ".(an)*" banan)
    (apple ".(an)*" a)
    ("ugli fruit" ".(an)*" u)
    ("ugli fruit" "[o-z](Plus(Range a z))" ugli) |}]

let%expect_test "arithmetic grammar" =
  let module Char_input = struct
    type element = char [@@deriving sexp_of]
    type collection = string

    let equal = Char.equal
    let ( <= ) = Char.( <= )
    let length = String.length
    let get = String.get
  end in
  let module Regex = Regex.Make (Char_input) in
  let module Grammar =
    Grammar_Make (Char_input) (Regex)
      (struct
        type t = string
        (* type collection = string list *)

        (* let append s list = s :: list *)
      end)
  in
  let module Interpreter = Interpreter (Grammar) in
  let grammar =
    let open Grammar in
    let rec seqs = function
      | [a] -> a
      | x :: a -> Regex.Seq (x, seqs a)
      | [] -> failwith "nonempty list required" in
    let numd = Regex.Range ('0', '9') in
    create ()
    |> add_rule Regex.(Plus (Char ' ')) "whitespace"
    |> add_rule Regex.(Char '(') "Lparen"
    |> add_rule Regex.(Char ')') "Rparen"
    |> add_rule Regex.(Char '+') "Operator"
    |> add_rule Regex.(Char '*') "Operator"
    |> add_rule Regex.(Plus (Range ('0', '9'))) "integer"
    |> add_rule
         Regex.(seqs [Star numd; Char '.'; Plus (Range ('0', '9'))])
         "float"
    |> add_rule Regex.(Plus (Range ('a', 'z'))) "id" in
  (let source = "(234 + 34.45) * 2" in
   match Interpreter.recognize grammar 0 source [] with
   | Ok items ->
       List.iter
         ~f:(fun (start, _end, action) ->
           Stdio.printf "%12s | %s\n" action
             (String.sub ~pos:start ~len:(_end - start) source) )
         items
   | Error s -> Stdio.print_endline s ) ;
  [%expect
    {|
          Lparen | (
         integer | 234
      whitespace |
        Operator | +
      whitespace |
           float | 34.4
         integer | 5
          Rparen | )
      whitespace |
        Operator | *
      whitespace |
         integer | 2 |}]
