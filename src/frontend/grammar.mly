%{
open Coral_core.Ast
module Type = Coral_core.Type
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> OPERATOR OPERADD OPERMUL OPERCMP
%token <string> IDENTIFIER STRING
%token <char> OTHER CHAR
%token <int> NEWLINE
%token FUNC IF ELSE ELIF FOR IN RETURN LET SET IMPORT EXTERN
%token LPAREN RPAREN COLON LBRACE RBRACE LBRACKET RBRACKET
%token INDENT DEDENT TYPE
%token COMMA ELLIPSIS SEMICOLON
%token EQ DOT
%token EOF

%left EQ OPERCMP
%left OPERATOR
%left OPERADD
%left OPERMUL

%start main
%type <Coral_core.Ast.node> main
%%

main
: newlines? x=lines EOF { Make.moduleNode x }
| newlines? x=lines_nl EOF { Make.moduleNode x }

newlines
: NEWLINE+ { }

lines_nl
: terminated_line { [$1] }
|  lines_nl NEWLINE { $1 }
| lines_nl terminated_line { $1 @ [$2] }

lines
: lines_nl line { $1 @ [$2] }

(* Why isn't a line just an expression? Maybe things like statements and
declaractions should just be expressions. But perhaps this way the parse errors
might be more understandable *)
line
: func_declaration { $1 }
| LET name=IDENTIFIER EQ e=expr { mm @@ Let({name=name;varType=None}, e) }
| LET name=IDENTIFIER COLON t=typedef EQ e=expr {
    mm @@ Let({name=name;varType=Some(t)}, e)
    }
| SET expr_op_unit EQ e=expr { mm @@ Set({name="?";varType=None}, e) }
| SET expr_op_unit OPERATOR expr { mm @@ Empty }
| RETURN arg=expr { mm @@ Return arg }
| RETURN { mm @@ Return(mm @@ Tuple []) }
| e=expr { e }
| e=forexpr {e}

terminated_line
: line NEWLINE { $1 }
| ifexpr { $1 }
| type_definition NEWLINE { $1 }
                    ;
/* func_name */
/* : IDENTIFIER { $1 } */
/* | IDENTIFIER LBRACKET typedef RBRACKET { $1 } */

(* path_segment represents a type or module name *)
path_name
: IDENTIFIER { [$1] }
| path_name DOT n=IDENTIFIER { $1 @ [n] }

func_return
: COLON ret=typedef { ret }
func_params
: LPAREN p=paramlist RPAREN { p }
func_body
: block_or_line { $1 }
func_declaration
: FUNC name=path_name ret=func_return? p=func_params body=func_body {Make.funcNode (List.nth name 0, ret, p, body) }

ifexpr
    : IF cond=binary_op_expr ifbody=block_or_line NEWLINE
    elif=elifexpr*
    elsebody=elseexpr? {
    let elsebody = match elsebody with
      | Some e -> e
      | None -> mm @@ Empty in
    Make.ifNode cond ifbody elif elsebody}
elifexpr
    : ELIF cond=binary_op_expr body=block_or_line NEWLINE { (cond, body) }
elseexpr
    : ELSE body=block_or_line NEWLINE { body }

forexpr
    : FOR LPAREN paramlist RPAREN IN binary_op_expr block_or_line { mm @@ Empty }

block
    : newlines? lines=lines DEDENT { mm @@ Block(lines) }
    | newlines? lines=lines_nl DEDENT { mm @@ Block(lines) }
block_or_line
    : COLON NEWLINE INDENT e=block { e }
    | COLON e=line { e }

(* Higher Precedence than calling *)
expr_atom
    : e=INTEGER { mm @@ IntLiteral e }
    | ELLIPSIS { mm @@ Var {name="..."; varType=None} }
    | e=FLOAT { mm @@ FloatLiteral e }
    | e=CHAR { mm @@ CharLiteral e }
    | e=IDENTIFIER { mm @@ Var {name=e; varType=None} }
    /* | e=IDENTIFIER COLON LBRACKET params=typedefList RBRACKET */
    /*     { Var {name=e; target=None; varType=None} } */
    | e=STRING { mm @@ StringLiteral e }
    | LPAREN e=e_exprlist RPAREN { match e with | [x] -> x | e -> mm @@ Tuple e }
    | LBRACKET e=e_exprlist RBRACKET { mm @@ List e }
    | e=member { e }
    /* | e=expr_atom COLON typedef { e } */

(* Higher Precedence than operators *)
expr_op_unit
    : e=expr_atom { e }
(* so we can do this in the grammar, by making a expr_atom_but_not_tuple pattern
   but doing it inside the reduction seems okay too *)
    | callee=expr_op_unit arg=expr_atom {
        match arg with
        | Tuple args, _ -> Make.callNode callee args
        | arg -> Make.callNode callee [arg] }

binary_op_expr
    : e=expr_op_unit { e }
    | lhs=binary_op_expr op=OPERMUL rhs=binary_op_expr { Make.binop (op, lhs, rhs) }
    | lhs=binary_op_expr op=OPERADD rhs=binary_op_expr { Make.binop(op, lhs, rhs) }
    | lhs=binary_op_expr op=OPERATOR rhs=binary_op_expr { Make.binop(op, lhs, rhs) }
    | l=binary_op_expr o=OPERCMP r=binary_op_expr { Make.binop (o, l, r) }
    | l=binary_op_expr EQ r=binary_op_expr { Make.binop ("=", l, r) }

(* expr contains binary_op_expr but also contains the conflict-y foo:type
it tends to eat up the colon, thus causing sentences expanding out to "expr" COLON NEWLINE to fail.
This is why for example, in if/elif/while block we use cond=binary_op_expr to exclude this pattern
so that the following colon newline will match.

In other places, we might be able to get away with requiring parentheses. *)
expr
    : e=binary_op_expr { e }
    | expr_atom COLON typedef { $1 }
    | IMPORT path=separated_nonempty_list(DOT, IDENTIFIER) { mm @@ Import {path;names=[`Module None]} }
    | IMPORT path=separated_nonempty_list(DOT, IDENTIFIER) LPAREN RPAREN { mm @@ Import {path;names=[`All]} }
    | EXTERN LPAREN fftype=STRING COMMA name=STRING COMMA _type=typedef RPAREN {
        Make.extern fftype name _type  }

typedef
    : IDENTIFIER { Coral_core.Type.Name $1 }
    | typedef DOT IDENTIFIER { Coral_core.Type.Dotted ($1, $3) }
    | typedef LBRACKET separated_list(COMMA, typedef) RBRACKET { Coral_core.Type.Parameterized($1, $3) }
    | ELLIPSIS { Coral_core.Type.Name "..." }

e_exprlist : { [] } | exprlist { $1 }

exprlist
    : e=expr { [e] }
    | x=exprlist COMMA e=expr { x@[e] }

paramlist
    : separated_list(COMMA, param) {List.mapi (fun i (name, typ) -> mm @@ Param {idx=i; name; typ}) $1}

param
    : IDENTIFIER COLON typedef { ($1, Some $3) }
    | IDENTIFIER { ($1, None) }

member
    : base=expr_atom DOT member=IDENTIFIER
      { mm @@ Member { base=base; memberName=member; } }

type_definition
    : TYPE _name=IDENTIFIER EQ _metatype=IDENTIFIER LBRACE separated_list_trailing(SEMICOLON, typedecl_field) RBRACE { mm @@ Empty }

(* Like separated_list, but we can accept a trailing delimiter as well.  Not
   sure how to write this; the simple pattern "separated_list(S, X) S?" doesn't
   work because by the time the S gets shifted, you're already in a
   separated_list loop.  *)
separated_list_trailing(S, X)
    : separated_list_trailing_x(S, X) { $1 }
    | separated_list_trailing_s(S, X) { $1 }
separated_list_trailing_x(S, X)
    : X { [$1] }
    | separated_list_trailing_s(S, X) X { $1 @ [$2] }
separated_list_trailing_s(S, X)
    : separated_list_trailing_x(S, X) S { $1 }
    | separated_list_trailing_s(S, X) S { $1 }

typedecl_field
    : IDENTIFIER COLON typedef { mm @@ Empty }
    | IDENTIFIER COLON typedef EQ expr { mm @@ Empty }
