type coraltype =
  | Free
  | Type of string
  | Parameterized of (string * string list)
  | Dotted of coraltype list

type varNode = {name:string; mutable target: node option}
and defNode = {name:string; defType:coraltype option}
and node =
  | Module of (node list)
  | Func of string * coraltype * defNode list * node
  | Comment of (string)
  | Binop of (string * node * node)
  | If of (node * node * node)
  | IntLiteral of string
  | FloatLiteral of string
  | StringLiteral of string
  | Var of varNode
  | Def of defNode
  | Block of node list
  | Call of node * node list
  | Tuple of (node list)
  | Return of node
  | Empty

let nodeName = function
  | Module _ -> "Module"
  | Func _ -> "Func"
  | Comment _ -> "Comment"
  | If _ -> "If"
  | IntLiteral _ -> "IntLiteral"
  | FloatLiteral _ -> "FloatLiteral"
  | StringLiteral _ -> "StringLiteral"
  | Var _ -> "Var"
  | Def _ -> "Def"
  | Block _ -> "Block"
  | Call _ -> "Call"
  | Tuple _ -> "Tuple"
  | Return _ -> "Return"
  | Empty -> "Empty"
  | Binop _ -> "Binop"

open Printf

let show_indent n = for i = 1 to n do printf "  " done

let needs_parentheses_for_call = function
  | Binop _ -> true
  | Tuple _ -> true
  | Call _ -> true
  | _ -> false

let rec show1 indent (is_inline:bool) node =
  match node with
  | Block lines -> List.iter (show1 indent is_inline) lines
  | If (cond, ifbody, elsebody) ->
     if not is_inline then show_indent indent;
     printf "if ";
     show1 0 true cond;
     printf ":\n";
     show1 (indent + 1) is_inline ifbody;
     (match elsebody with
      | Empty -> ()
      | _ ->
        if not is_inline then show_indent indent;
        printf "else:\n";
        show1 (indent + 1) is_inline elsebody)
  | Func (name, ret_type, params, body) ->
     printf "func %s(" name;
     let rec loop = function
       | [] -> ()
       | p :: xs ->
          show1 0 true (Def p);
          match xs with | [] -> () | _ -> printf ", ";
          loop xs
     in loop params;
     printf "):\n";
     show1 (indent + 1) is_inline body
  | _ ->
    if not is_inline then show_indent indent;
    (match node with
    | Module (lines) ->
       List.iter (show1 indent is_inline) lines
    | Def x -> printf "%s" x.name
    | Var s -> printf "%s" s.name
    | StringLiteral s ->
       let r = Str.regexp "\n" in
       printf "\"%s\"" (Str.global_replace r "\\n" s)
    | IntLiteral n -> printf "%s" n
    | FloatLiteral n -> printf "%s" n
    | Comment c -> printf "# %s" c
    | Binop(op, lhs, rhs) ->
       show1 0 true lhs;
       printf " %s " op;
       show1 0 true rhs;
    | Call(callee, args) ->
       show1 0 true callee;
       (match args with
        | [n] ->
           printf " ";
           if needs_parentheses_for_call n then printf "(";
           show1 0 true n;
           if needs_parentheses_for_call n then printf ")"
        | _ ->
          printf "(";
          for i = 0 to List.length args - 1 do
            if i > 0 then printf ", ";
            show1 0 true (List.nth args i);
          done;
          printf ")"
       )
    | Return v ->
       printf "return ";
       show1 0 true v
    | Tuple l ->
       printf "Tuple()";
    | Func _ -> printf "func";
    | If _ -> printf "func";
    | Block _ -> printf "func";
    | Empty -> printf "empty";
    );
    if not is_inline then printf "\n"

let show = show1 0 false
