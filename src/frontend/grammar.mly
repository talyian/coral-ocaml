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
%token FUNC IF ELSE ELIF FOR IN RETURN LET SET
%token LPAREN RPAREN COLON LBRACE RBRACE LBRACKET RBRACKET
%token INDENT DEDENT TYPE
%token COMMA ELLIPSIS SEMICOLON
%token EQ DOT
%token EOF

%left EQ OPERCMP
%left OPERATOR
%left OPERADD
%left OPERMUL

%nonassoc COLON
%nonassoc MORE_THAN_COLON

%start main
%type <Coral_core.Ast.node> main
%%

main
    : newlines? x=lines EOF { Module(Make.moduleNode x) }
    | newlines? x=lines_nl EOF { Module(Make.moduleNode x) }

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
    | LET name=IDENTIFIER EQ e=expr { Let({name=name;varType=None}, e) }
    | LET name=IDENTIFIER COLON t=typedef EQ e=expr {
        Let({name=name;varType=Some(t)}, e)
      }
    | SET expr_op_unit EQ e=expr { Set({name="?";varType=None}, e) }
    | SET expr_op_unit OPERATOR expr { Empty }
    | RETURN arg=expr { arg }
    | RETURN { Tuple [] }
    | e=expr { e }
    | e=forexpr {e}

terminated_line
    : line NEWLINE { $1 }
    | ifexpr { $1 }
    | type_definition NEWLINE { $1 }
                        ;
func_name
    : IDENTIFIER { $1 }
    | IDENTIFIER LBRACKET typedef RBRACKET { $1 }

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
    : FUNC name=path_name ret=func_return? p=func_params body=func_body {Func(Make.funcNode (List.nth name 0, ret, p, body)) }

ifexpr
    : IF cond=binary_op_expr ifbody=block_or_line NEWLINE
      elifexpr*
      elsebody=elseexpr? { If(cond, ifbody, Option.value elsebody ~default:Empty) }
elifexpr
    : ELIF cond=binary_op_expr body=block_or_line NEWLINE { (cond, body) }
elseexpr
    : ELSE body=block_or_line NEWLINE { body }

forexpr : FOR LPAREN paramlist RPAREN IN expr block_or_line { Empty }

block
    : newlines? lines=lines DEDENT { Block(lines) }
    | newlines? lines=lines_nl DEDENT { Block(lines) }
block_or_line
    : COLON NEWLINE INDENT e=block { e }
    | COLON e=line { e }

(* Higher Precedence than calling *)
expr_atom
    : e=INTEGER { IntLiteral e }
    | e=FLOAT { FloatLiteral e }
    | e=CHAR { CharLiteral e }
    | e=IDENTIFIER { Var {name=e; varType=None} }
    /* | e=IDENTIFIER COLON LBRACKET params=typedefList RBRACKET */
    /*     { Var {name=e; target=None; varType=None} } */
    | e=STRING { StringLiteral e }
    | LPAREN e=e_exprlist RPAREN { match e with | [x] -> x | e -> Tuple e }
    | LBRACKET e=e_exprlist RBRACKET { List e }
    | e=member { e }
    | e=expr_atom COLON typedef { e }

(* Higher Precedence than operators *)
expr_op_unit
    : e=expr_atom %prec MORE_THAN_COLON { e }
    | callee=expr_op_unit arg=expr_atom %prec MORE_THAN_COLON {Call (Make.callNode callee [arg])}

binary_op_expr
    : e=expr_op_unit { e }
    | lhs=binary_op_expr op=OPERMUL rhs=binary_op_expr { Make.binop (op, lhs, rhs) }
    | lhs=binary_op_expr op=OPERADD rhs=binary_op_expr { Make.binop(op, lhs, rhs) }
    | lhs=binary_op_expr op=OPERATOR rhs=binary_op_expr { Make.binop(op, lhs, rhs) }
    | l=binary_op_expr o=OPERCMP r=binary_op_expr { Make.binop (o, l, r) }
    | l=binary_op_expr EQ r=binary_op_expr { Make.binop ("=", l, r) }


expr
    : e=binary_op_expr { e }

typedef
    : IDENTIFIER { Coral_core.Type.Name "" }
    | typedef DOT IDENTIFIER { $1 }
    | typedef LBRACKET typedef RBRACKET { $1 }

e_exprlist : { [] } | exprlist { $1 }

exprlist
    : e=expr { [e] }
    | x=exprlist COMMA e=expr { x@[e] }

paramlist : separated_list(COMMA, param) { $1 }
param
    : IDENTIFIER COLON typedef { Empty }
    | IDENTIFIER { Empty }

member
    : base=expr_atom DOT member=IDENTIFIER
      { Member { base=base; memberName=member; } }

type_definition
    : TYPE name=IDENTIFIER EQ metatype=IDENTIFIER LBRACE separated_list_trailing(SEMICOLON, typedecl_field) RBRACE { Empty }

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
    : IDENTIFIER COLON typedef { Empty }
    | IDENTIFIER COLON typedef EQ expr { Empty }
