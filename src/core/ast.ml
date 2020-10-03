open Base

type 'a coraltype = 'a Type.t [@@deriving show, sexp_of]

type 'a varInfo = {
  name : string;
  mutable varType : 'a coraltype option;
  mutable target : 'a option;
}
[@@deriving show, sexp_of]

type 'a funcInfo = {
  name : string;
  ret_type : 'a coraltype option;
  params : 'a list;
  body : 'a;
}
[@@deriving show, sexp_of]

type 'a multifuncInfo = {
  name : string;
  (* mutable func : 'a ref;
   * mutable next : 'a ref option; *)
}
[@@deriving show, sexp_of]

type 'a moduleInfo = { mutable name : string; lines : 'a list }
[@@deriving show, sexp_of]

type 'a tupleInfo = { name : string; fields : (string * 'a coraltype) list }
[@@deriving show, sexp_of]

type 'a memberInfo = {
  base : 'a;
  memberName : string;
  (* mutable basetype : 'a coraltype;
   * mutable memberIndex : int; *)
}
[@@deriving show, sexp_of]

type 'a callInfo = {
  callee : 'a;
  args : 'a list;
  name : string;
  (* (\* The type solver sets this if the callee is a multifunc *\)
   * mutable coraltype : 'a coraltype option;
   * mutable overloadIndex : int; *)
}
[@@deriving show, sexp_of]

type node =
  | Module of node moduleInfo
  | Func of node funcInfo
  | Multifunc of node multifuncInfo
  | Comment of string
  | Binop of node callInfo
  | If of (node * node * node)
  | IntLiteral of string
  | FloatLiteral of string
  | CharLiteral of char
  | StringLiteral of string
  | Var of node varInfo
  | Let of node varInfo * node
  | Set of node varInfo * node
  | Block of node list
  | Call of node callInfo
  | Tuple of node list
  | List of node list
  | TupleDef of node tupleInfo
  | Member of node memberInfo
  | Return of node
  | Empty
[@@deriving show, sexp_of]

let nodeName = function
  | Module _ -> "Module"
  | Func _ -> "Func"
  | Comment _ -> "Comment"
  | If _ -> "If"
  | IntLiteral _ -> "IntLiteral"
  | FloatLiteral _ -> "FloatLiteral"
  | StringLiteral _ -> "StringLiteral"
  | Var _ -> "Var"
  | Block _ -> "Block"
  | Call _ -> "Call"
  | Tuple _ -> "Tuple"
  | Return _ -> "Return"
  | Empty -> "Empty"
  | Binop _ -> "Binop"
  | Let _ -> "Let"
  | Set _ -> "Set"
  | Multifunc _ -> "Multifunc"
  | Member _ -> "Member"
  | TupleDef _ -> "TupleDef"
  | List _ -> "List"
  | CharLiteral _ -> "CharLiteral"

let show_indent n =
  for _ = 1 to n do
    Stdio.printf "  "
  done

let needs_parentheses_for_call = function
  | Binop _ -> true
  | Tuple _ -> true
  | Call _ -> true
  | _ -> false

let string_escape = String.escaped

let string_name_escape s =
  Re.replace_string (Re.Pcre.regexp {|\n|\t| |}) ~by:"_" s

module Make = struct
  let binop (op, lhs, rhs) =
    Binop
      {
        name = op;
        callee = Empty;
        args = [ lhs; rhs ];
      }

  let callNode callee args =
    { name = "?"; callee; args; }

  let moduleNode lines = { name = "module"; lines }

  let funcNode (name, ret_type, params, body) =
    { name; ret_type; params; body }
end
