%{
open Coral_core.Ast
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> OPERATOR OPERADD OPERMUL OPERCMP
%token <string> IDENTIFIER STRING
%token <char> OTHER CHAR
%token <int> NEWLINE
%token FUNC IF ELSE ELIF FOR IN COMMA ELLIPSIS RETURN LET SET
%token LPAREN RPAREN COLON LBRACE RBRACE LBRACKET RBRACKET
%token INDENT DEDENT TYPE
%token EQ DOT
%token EOF

%start main
%type <Coral_core.Ast.node> expr line block main
%type <Coral_core.Ast.node list> lines
%type <Coral_core.Ast.node list> paramlist
%%

main
  : newlines? x=lines EOF { Module(make_module x) }
  | newlines? x=lines_nl EOF { Module(make_module x) }

newlines: NEWLINE+ { }

lines
                : line { [$1] }
                | lines_nl line { $1 @ [$2] }
lines_nl : lines newlines { $1 }
line
  : func_declaration { $1 }
  | LET name=IDENTIFIER EQ e=expr { Let({name=name;target=None;varType=None}, e) }
  | LET name=IDENTIFIER COLON t=typedef EQ e=expr {
      Let({name=name;target=None;varType=Some(t)}, e)
    }
  | SET expr_op_unit EQ e=expr { Set({name="?";target=None;varType=None}, e) }
  | SET expr_op_unit OPERATOR expr { Empty }
  | RETURN arg=expr { Return {node=arg;coraltype=None} }
  | RETURN { Return {node=Tuple [];coraltype=None} }
  | e=expr { e }
  | e=ifexpr { e }
  | e=forexpr {e}
func_name
  : IDENTIFIER { $1 }
  | IDENTIFIER LBRACKET typedef RBRACKET { $1 }

func_return : COLON ret=typedef { ret }
func_params : LPAREN p=paramlist RPAREN { p }
func_body   : block_or_line { $1 }
func_declaration
  : FUNC name=func_name ret=func_return? p=func_params body=func_body
    { let ret = match ret with | Some (v) -> v | None -> Type "" in
      Func(newFunc (name, ret, p, body)) }

ifexpr
  : IF cond=expr2 ifbody=block_or_line NEWLINE
    elifexpr*
    elsebody=elseexpr? { If(cond, ifbody, Option.value elsebody ~default:Empty) }
elifexpr
  : ELIF cond=expr2 body=block_or_line NEWLINE { (cond, body) }
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
  | e=IDENTIFIER { Var {name=e; target=None; varType=None} }
  /* | e=IDENTIFIER COLON LBRACKET params=typedefList RBRACKET */
  /*     { Var {name=e; target=None; varType=None} } */
  | e=STRING { StringLiteral e }
  | LPAREN e=e_exprlist RPAREN { match e with | [x] -> x | e -> Tuple e }
  | LBRACKET e=e_exprlist RBRACKET { match e with | [x] -> x | e -> List e }
  | e=member { Member e }
(* Higher Precedence than operators *)
expr_op_unit
  : e=expr_atom { e }
  | callee=expr_op_unit arg=expr_atom { Call (callNode callee [arg]) }

expr0
  : e=expr_op_unit { e }
  | lhs=expr0 op=OPERMUL rhs=expr0 { binop (op, lhs, rhs) }

expr1
  : e=expr0 { e }
  | lhs=expr1 op=OPERADD rhs=expr1 { binop(op, lhs, rhs) }
  | lhs=expr1 op=OPERATOR rhs=expr1 { binop(op, lhs, rhs) }

expr2
  : e=expr1 { e }
  | l=expr2 o=OPERCMP r=expr2 { binop (o, l, r) }
  | l=expr2 EQ r=expr2 { binop ("=", l, r) }

expr : e=expr2 { e }

e_exprlist : { [] } | exprlist { $1 }
exprlist
  : e=expr { [e] }
  | x=exprlist COMMA e=expr { x@[e] }

param
  : e=IDENTIFIER { Def {name=e; defType=None} }
  | e=IDENTIFIER COLON t=typedef { Def {name=e; defType=Some t} }
  | ELLIPSIS { Def {name="..."; defType=Some (Type "...") } }
paramlist
  : e=non_empty_paramlist { e }
  | { [] }
non_empty_paramlist
  : p=param { [p] }
  | x=paramlist COMMA p=param { x@[p] }

typedef
  : e=IDENTIFIER { Type e }
  | ELLIPSIS { Type "..." }
  | e=IDENTIFIER LBRACKET params=typedefList RBRACKET { Parameterized(e, params) }
typedefList : separated_nonempty_list(COMMA, typedef) { $1 }

member
: base=expr_atom DOT member=IDENTIFIER
  { { base=base; memberName=member; basetype=Type ""; memberIndex=0} }
