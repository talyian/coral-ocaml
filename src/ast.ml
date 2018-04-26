type coraltype =
  | Free
  | Type of string
  | Parameterized of (string * coraltype list)
  | Dotted of coraltype list

let rec type_to_string = function
  | Free -> "free"
  | Type s -> s
  | Parameterized (name, params) -> name ^ "[" ^ String.concat ", " (List.map type_to_string params) ^ "]"
  | Dotted (n) -> "dotted"

type 'a varInfo = {
  name:string;
  mutable varType: coraltype option;
  mutable target: 'a option;
}

type 'a defInfo = {
    name: string;
    mutable defType: coraltype option;
}

type 'a typeInfo = {
  node: 'a;
  mutable coraltype: coraltype option;
}

type 'a funcInfo = {
  name: string;
  mutable ret_type: coraltype;
  params: ('a defInfo) list;
  body: 'a
}
and node =
  | Module of (node list)
  | Func of node funcInfo
  | Multifunc of string * (node funcInfo) list
  | Comment of (string)
  | Binop of (string * node * node)
  | If of (node * node * node)
  | IntLiteral of string
  | FloatLiteral of string
  | StringLiteral of string
  | Var of node varInfo
  | Def of node defInfo
  | Let of node varInfo * node
  | Set of node varInfo * node
  | Block of node list
  | Call of node * node list
  | Tuple of (node list)
  | Return of node typeInfo
  | Empty
type defNode = node defInfo

let newFunc (name, ret, params, body) = {
    name=name;
    ret_type=ret;
    params=params;
    body=body }

let nodeName = function | Module _ -> "Module"  | Func _ -> "Func"  | Comment _ -> "Comment"  | If _ -> "If"  | IntLiteral _ -> "IntLiteral"  | FloatLiteral _ -> "FloatLiteral"  | StringLiteral _ -> "StringLiteral"  | Var _ -> "Var"  | Def _ -> "Def"  | Block _ -> "Block"  | Call _ -> "Call"  | Tuple _ -> "Tuple"  | Return _ -> "Return"  | Empty -> "Empty"  | Binop _ -> "Binop"  | Let _ -> "Let" | Set _ -> "Set" | Multifunc _ -> "Multifunc"

open Printf

let show_indent n = for i = 1 to n do printf "  " done

let needs_parentheses_for_call = function
  | Binop _ -> true
  | Tuple _ -> true
  | Call _ -> true
  | _ -> false

let string_escape s = s
  |> Str.global_replace (Str.regexp "\n") "\\n"
  |> Str.global_replace (Str.regexp "\t") "\\t"

let string_name_escape s = s
  |> Str.global_replace (Str.regexp "\n\|\t\| ") "_"
                                                 
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
  | Multifunc (name, funcs) ->
     List.iter (show1 indent is_inline) (List.map (fun f -> Func f) funcs)
  | Func {name=name; ret_type=ret_type; params=params; body=body} ->
     (match ret_type with
      | Type "" -> printf "func %s(" name
      | tt -> printf "func %s: %s(" name (type_to_string ret_type));
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
    | Def x ->
       (match x.defType with
       | None -> printf "%s" x.name
       | Some(t) -> printf "%s: %s" x.name (type_to_string t))
    | Var s ->
       (match s.varType with
        | None -> printf "%s" s.name
        | Some(t) -> printf "%s: %s" s.name (type_to_string t))
    | StringLiteral s -> printf "\"%s\"" (string_escape s)
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
       show1 0 true (v.node)
    | Let(var, value) ->
       printf "let ";
       show1 0 true (Var var);
       printf " = ";
       show1 0 true value;
    | Set(var, value) ->
       printf "set ";
       show1 0 true (Var var);
       printf " = ";
       show1 0 true value;
    | Tuple l -> printf "Tuple()";
    | Multifunc _ -> printf "multifunc";
    | Func _ -> printf "func";
    | If _ -> printf "if";
    | Block _ -> printf "block";
    | Empty -> printf "empty";
    );
    if not is_inline then printf "\n"

let show = show1 0 false
