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
  params: 'a list;
  body: 'a
}

type 'a moduleInfo = {
  mutable name: string;
  lines: 'a list;
}

type tupleInfo = {
  name: string;
  fields: (string * coraltype) list
}

type 'a memberInfo = {
  base: 'a;
  memberName: string;
  mutable basetype: coraltype;
  mutable memberIndex: int;
}

type 'a callInfo = {
  callee: 'a;
  args: 'a list;
  (* The type solver sets this if the callee is a multifunc *)
  mutable overloadIndex: int;
}
let callNode callee args = {callee=callee;args=args;overloadIndex=0}

let make_module lines = {name="module"; lines=lines}

type node =
  | Module of node moduleInfo
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
  | Call of node callInfo
  | Tuple of (node list)
  | TupleDef of tupleInfo
  | Member of node memberInfo
  | Return of node typeInfo
  | Empty
type defNode = node defInfo

let newFunc (name, ret, params, body) = {
    name=name;
    ret_type=ret;
    params=params;
    body=body }

let nodeName = function | Module _ -> "Module"  | Func _ -> "Func"  | Comment _ -> "Comment"  | If _ -> "If"  | IntLiteral _ -> "IntLiteral"  | FloatLiteral _ -> "FloatLiteral"  | StringLiteral _ -> "StringLiteral"  | Var _ -> "Var"  | Def _ -> "Def"  | Block _ -> "Block"  | Call _ -> "Call"  | Tuple _ -> "Tuple"  | Return _ -> "Return"  | Empty -> "Empty"  | Binop _ -> "Binop"  | Let _ -> "Let" | Set _ -> "Set" | Multifunc _ -> "Multifunc" | Member _ -> "Member" | TupleDef _ -> "TupleDef"

let show_indent n = for i = 1 to n do Printf.printf "  " done

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

let rec pprint1 fmt indent (is_inline:bool) node =
  let show1 = pprint1 fmt in
  let printf = Format.fprintf fmt in
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
      | Type "" -> Format.fprintf fmt "func %s(" name
      | tt -> Format.fprintf fmt "func %s: %s(" name (type_to_string ret_type));
     let rec loop = function
       | [] -> ()
       | Def p :: xs ->
          show1 0 true (Def p);
          (match xs with | [] -> () | _ -> printf ", "; loop xs)
       | _ -> failwith "oops"
     in loop params;
     printf "):\n";
     show1 (indent + 1) is_inline body
  | _ ->
    if not is_inline then show_indent indent;
    (match node with
    | Module {lines=lines} ->
       List.iter (show1 indent is_inline) lines
    | Def x ->
       (match x.defType with
       | None -> Format.fprintf fmt "%s" x.name
       | Some(t) -> Format.fprintf fmt "%s: %s" x.name (type_to_string t))
    | Var s ->
       (match s.varType with
        | None -> Format.fprintf fmt "%s" s.name
        | Some(t) -> Format.fprintf fmt "%s: %s" s.name (type_to_string t))
    | StringLiteral s -> Format.fprintf fmt "\"%s\"" (string_escape s)
    | IntLiteral n -> Format.fprintf fmt "%s" n
    | FloatLiteral n -> Format.fprintf fmt "%s" n
    | Comment c -> Format.fprintf fmt "# %s" c
    | Binop(op, lhs, rhs) ->
       show1 0 true lhs;
       Format.fprintf fmt " %s " op;
       show1 0 true rhs;
    | Call {callee=callee;args=args} ->
       show1 0 true callee;
       (match args with
        | [n] ->
           Format.fprintf fmt " ";
           if needs_parentheses_for_call n then Format.fprintf fmt "(";
           show1 0 true n;
           if needs_parentheses_for_call n then Format.fprintf fmt ")"
        | _ ->
          Format.fprintf fmt "(";
          for i = 0 to List.length args - 1 do
            if i > 0 then Format.fprintf fmt ", ";
            show1 0 true (List.nth args i);
          done;
          Format.fprintf fmt ")"
       )
    | Return v ->
       Format.fprintf fmt "return ";
       show1 0 true (v.node)
    | Let(var, value) ->
       Format.fprintf fmt "let ";
       show1 0 true (Var var);
       Format.fprintf fmt " = ";
       show1 0 true value;
    | Set(var, value) ->
       Format.fprintf fmt "set ";
       show1 0 true (Var var);
       Format.fprintf fmt " = ";
       show1 0 true value;
    | Tuple [] -> Format.fprintf fmt "Tuple()";
    | Tuple items ->
       Format.fprintf fmt "(";
       let rec looper = function
         | [] -> ()
         | [item] -> show1 0 true item
         | x :: xs -> show1 0 true x; Format.fprintf fmt ", "; looper xs
       in looper items; Format.fprintf fmt ")";
    | Multifunc _ -> Format.fprintf fmt "multifunc";
    | Func _ -> Format.fprintf fmt "func";
    | If _ -> Format.fprintf fmt "if";
    | Block _ -> Format.fprintf fmt "block";
    | Empty -> Format.fprintf fmt "empty";
    | TupleDef def ->
       Format.fprintf fmt "type %s = {" def.name;
       def.fields
       |> List.map (fun (name, ctype) -> Printf.sprintf "%s:%s" name (type_to_string ctype))
       |> String.concat ", "
       |> Format.fprintf fmt "%s";
       Format.fprintf fmt "}";
    | Member mem ->
       show1 0 true mem.base;
       Format.fprintf fmt ".%s" mem.memberName;
    );
    if not is_inline then printf "\n"

let show = pprint1 Format.std_formatter 0 false
let string_of_node x =
  pprint1 Format.str_formatter 0 false x;
  Format.flush_str_formatter ()
