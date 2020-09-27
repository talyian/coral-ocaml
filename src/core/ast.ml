open Base

type coraltype =
  | Free
  | Type of string
  | Parameterized of (string * coraltype list)
  | Dotted of coraltype list

let rec type_to_string = function
  | Free -> "free"
  | Type s -> s
  | Parameterized (name, params) -> name ^ "[" ^ String.concat ~sep:", " (List.map ~f:type_to_string params) ^ "]"
  | Dotted (_) -> "dotted"

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

type 'a multifuncInfo = {
  name: string;
  mutable func: 'a ref;
  mutable next: 'a ref option;
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
  name: string;
  (* The type solver sets this if the callee is a multifunc *)
  mutable coraltype: coraltype option;
  mutable overloadIndex: int;
}

let callNode callee args = {name="?";callee=callee;args=args;overloadIndex=0;coraltype=None}

let make_module lines = {name="module"; lines=lines}

type node =
  | Module of node moduleInfo
  | Func of node funcInfo
  | Multifunc of node multifuncInfo
  | Comment of (string)
  | Binop of node callInfo
  | If of (node * node * node)
  | IntLiteral of string
  | FloatLiteral of string
  | CharLiteral of char
  | StringLiteral of string
  | Var of node varInfo
  | Def of node defInfo
  | Let of node varInfo * node
  | Set of node varInfo * node
  | Block of node list
  | Call of node callInfo
  | Tuple of (node list)
  | List of (node list)
  | TupleDef of tupleInfo
  | Member of node memberInfo
  | Return of node typeInfo
  | Empty
[@@deriving show, sexp, fields]

type defNode = node defInfo

let newFunc (name, ret, params, body) = {
    name=name;
    ret_type=ret;
    params=params;
    body=body }

let nodeName = function | Module _ -> "Module"  | Func _ -> "Func"  | Comment _ -> "Comment"  | If _ -> "If"  | IntLiteral _ -> "IntLiteral"  | FloatLiteral _ -> "FloatLiteral"  | StringLiteral _ -> "StringLiteral"  | Var _ -> "Var"  | Def _ -> "Def"  | Block _ -> "Block"  | Call _ -> "Call"  | Tuple _ -> "Tuple"  | Return _ -> "Return"  | Empty -> "Empty"  | Binop _ -> "Binop"  | Let _ -> "Let" | Set _ -> "Set" | Multifunc _ -> "Multifunc" | Member _ -> "Member" | TupleDef _ -> "TupleDef" | List _ -> "List"

let show_indent n = for _ = 1 to n do Stdio.printf "  " done

let needs_parentheses_for_call = function
  | Binop _ -> true
  | Tuple _ -> true
  | Call _ -> true
  | _ -> false

let string_escape = String.escaped

let string_name_escape s =
  Re.replace_string (Re.Pcre.regexp {|\n|\t| |}) ~by:"_" s

let binop (op, lhs, rhs) = Binop {
  name=op;
  callee=Empty;
  args=[lhs;rhs];
  coraltype=None;
  overloadIndex=0
}
