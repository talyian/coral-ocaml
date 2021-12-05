module Grammar (Terminal : sig
  type t
end) =
struct
  type terminal = Terminal.t
  type nonterminal = string
  type symbol = Terminal of terminal | Nonterminal of nonterminal

  type expr =
    | Terminal of terminal
    | Range of terminal * terminal
    | Nonterminal of nonterminal
    | Any_terminal
    | Empty
    | Seq of expr * expr
    | Or of expr * expr
    | Star of expr
    | Plus of expr

  type expr_kind =
    | (* this expression is non-recursive or only tail recursive *) Regular
    | (* This expression is recursive *) ContextFree

  let create () = []
  let add_nonterminal g name = (g, name)
  let add_rule grammar target expr = (target, expr) :: grammar
end

let%expect_test "sexp grammar" =
  (*
space :  ' '
lparen : '('
rparen : ')'
digit    :  range '0' '9'
idchar   :  digit | range 'a' 'z' | range 'A' 'Z' | '-'
                                    { $$ }
integer  :  digit+                  { Int.of_string $$ }
float    :  digit+ '.' digit*       { Float.of_string $$ }
float    :  '.' digit+              { Float.of_string $$ }
word     :  idchar+                 { $$ }
qword    : '"' _* '"'               { $$ }

atom : integer { Atom.Int    $1 }
atom : float   { Atom.Float  $1 }
atom : qword   { Atom.String $1 }
atom : word    { Atom.String $1 } 

expr : atom    { $1 }
expr : list    { $1 }

list : '[' ']' { Nil }
list : '[' list_content ']' { $2 }
list_content : expr ',' list_content { $1 :: $3 }
list_content : expr                  { [$1] }


expr : digit+            { Atom.Int @@ Int.of_string $$ }
expr : digit+ '.' digit* { Atom.Float @@ Float.of_string $$ }
expr : '.' digit+        { Atom.Float @@ Float.of_string $$ }
expr : '"' ('\\' '"' | not '"' )* '"'
                         { Atom.String $$ }
expr : idchar+           { Atom.String $$ }
expr : '[' ']'           { List [] }
expr : '[' expr ']'
expr : '[' expr ']'
*)

  (* let open Grammar (struct type t = char end) in *)
  (* let grammar = create () in *)
  (* let grammar, expr = add_nonterminal grammar "expr" in *)
  (* let grammar = add_rule grammar expr (seqs [   *)
  ()
