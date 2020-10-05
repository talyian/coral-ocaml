open Base

(*
   The main type here is node.
   Before that, here are all the variant types for node: these are all parameterized with 'a = node.
   Why I did this instead of making them all recursive, I don't remember.
*)
type 'a coraltype = 'a Type.t [@@deriving show, sexp_of]

type 'a varInfo = { name : string; varType : 'a coraltype option }
[@@deriving sexp_of]

(* we customize the impl for show_varInfo just to make it slightly less verbose *)
let pp_varInfo (f : Formatter.t -> 'a -> unit) (fmt : Formatter.t)
    (v : 'a varInfo) =
  Stdlib.Format.fprintf fmt "%s" v.name;
  match v.varType with None -> () | Some t -> pp_coraltype f fmt t

type 'a funcInfo = {
  name : string;
  ret_type : 'a coraltype option;
  params : 'a list;
  body : 'a;
}
[@@deriving show, sexp_of]

type 'a moduleInfo = { name : string; lines : 'a list }
[@@deriving show, sexp_of]

type 'a tupleInfo = { name : string; fields : (string * 'a coraltype) list }
[@@deriving show, sexp_of]

type 'a memberInfo = { base : 'a; memberName : string }
[@@deriving show, sexp_of]

type 'a callInfo = { callee : 'a; args : 'a list } [@@deriving show, sexp_of]

type node =
  | Module of node moduleInfo
  | Import of {
      path : string list;
      names :
        [ `Module of string option | `All | `Member of string * string option ]
        list;
    }
  | Func of node funcInfo
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
  | Import _ -> "Import"
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
  | Member _ -> "Member"
  | TupleDef _ -> "TupleDef"
  | List _ -> "List"
  | CharLiteral _ -> "CharLiteral"

(* let needs_parentheses_for_call = function
 *   | Binop _ -> true
 *   | Tuple _ -> true
 *   | Call _ -> true
 *   | _ -> false *)

(* let string_escape = String.escaped *)

(* let string_name_escape s =
 *   Re.replace_string (Re.Pcre.regexp {|\n|\t| |}) ~by:"_" s *)

(* some simplified constructors for ast nodes. I question whether these are really of any use *)
module Make = struct
  let binop (op_name, lhs, rhs) =
    let op = { name = op_name; varType = None } in
    Binop { callee = Var op; args = [ lhs; rhs ] }

  let callNode (callee : node) (args : node list) : node callInfo =
    { callee; args }

  let moduleNode lines = { name = "module"; lines }

  let funcNode (name, ret_type, params, body) = { name; ret_type; params; body }

  let extern ffitype name _type =
    Call
      {
        callee = Var { name = "extern"; varType = None };
        args = [ StringLiteral ffitype; Var { name; varType = Some _type } ];
      }
end

let recurse_unit (f : node -> unit) e =
  match e with
  | Module m -> List.iter ~f m.lines
  | Binop { callee; args } | Call { callee; args } ->
      f callee;
      List.iter ~f args
  | Tuple xs -> List.iter ~f xs
  | Let (_, b) -> f b
  | List xs -> List.iter ~f xs
  | Member { base; _ } -> f base
  | Block xs -> List.iter ~f xs
  | If (cond, ifbody, elsebody) ->
      f cond;
      f ifbody;
      f elsebody
  | Set (_, value) -> f value
  | Return v -> f v
  | TupleDef _ -> ()
  | Func { name = _; ret_type = _; params; body } ->
      List.iter ~f params;
      f body
  | Var _ | IntLiteral _ | FloatLiteral _ | CharLiteral _ | StringLiteral _
  | Comment _ | Empty | Import _ ->
      ()

(* | _ -> print_endline @@ nodeName e *)
