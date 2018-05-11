%{
open Ast
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> OPERATOR OPERADD OPERMUL OPERCMP
%token <string> IDENTIFIER STRING
%token <char> OTHER
%token <int> NEWLINE
%token FUNC IF ELSE COMMA ELLIPSIS RETURN LET SET
%token LPAREN RPAREN COLON LBRACE RBRACE LBRACKET RBRACKET
%token INDENT DEDENT TYPE
%token EQ DOT

%start main
%token EOF
%type <Ast.node> expr line block main
%type <Ast.node list> lines
%type <Ast.node list> paramlist
%%

main
  : x=lines EOF        { Module(make_module x) }
  | x=lines e=expr EOF { Module(make_module @@ x @ [e]) }

line
  : func_declaration { $1 }
  | LET name=IDENTIFIER EQ e=expr NEWLINE { Let({name=name;target=None;varType=None}, e) }
  | LET name=IDENTIFIER COLON t=typedef EQ e=expr NEWLINE {
      Let({name=name;target=None;varType=Some(t)}, e) }
  | SET name=IDENTIFIER EQ e=expr NEWLINE { Set({name=name;target=None;varType=None}, e) }
  | RETURN arg=expr NEWLINE { Return {node=arg;coraltype=None} }
  | RETURN NEWLINE { Return {node=Tuple [];coraltype=None} }
  | e=expr NEWLINE { e }
  | e=ifexpr {e}
  | e=tuple_def { TupleDef e }

func_name
  : IDENTIFIER { $1 }
  | IDENTIFIER LBRACKET typedef RBRACKET { $1 }

func_return : COLON ret=typedef { ret }
func_params : LPAREN p=paramlist RPAREN { p }
func_body   : block_or_line { $1 } | NEWLINE { Empty }
func_declaration
  : FUNC name=func_name ret=func_return? p=func_params body=func_body
    { let ret = match ret with | Some (v) -> v | None -> Type "" in
      Func(newFunc (name, ret, p, body)) }

ifexpr
  : IF cond=expr2 ifbody=block_or_line { If(cond, ifbody, Empty) }
  | IF cond=expr2 ifbody=block_or_line ELSE elsebody=block_or_line { If(cond, ifbody, elsebody) }
  | IF cond=expr2 ifbody=block_or_line ELSE elsebody=ifexpr { If(cond, ifbody, elsebody) }

lines
  : NEWLINE { [] }
  | line { [$1] }
  | x=lines e=line { x @ [e] }
  | x=lines NEWLINE { x }

block
  : COLON NEWLINE INDENT lines=lines DEDENT { Block(lines) }

block_or_line
  : e=block { e }
  | COLON e=expr NEWLINE { e }

(* Higher Precedence than calling *)
expr_atom
  : e=INTEGER { IntLiteral e }
  | e=FLOAT { FloatLiteral e }
  | e=IDENTIFIER { Var {name=e; target=None; varType=None} }
  | e=IDENTIFIER COLON LBRACKET params=typedefList RBRACKET
      { Var {name=e; target=None; varType=None} }
  | e=STRING { StringLiteral e }
  | LPAREN e=expr2 RPAREN { e }
  | e=member { Member e }
(* Higher Precedence than operators *)
expr_op_unit
  : e=expr_atom { e }
  | callee=expr_atom LPAREN args=exprlist RPAREN { Call (callNode callee args) }
  | callee=expr_atom LPAREN RPAREN { Call (callNode callee []) }
  | callee=expr_atom arg=expr_atom { Call (callNode callee [arg]) }

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
  : NEWLINE? INDENT? p=param { [p] }
  | x=paramlist COMMA NEWLINE? INDENT? p=param { x@[p] }

typedef
  : e=IDENTIFIER { Type e }
  | ELLIPSIS { Type "..." }
  | e=IDENTIFIER LBRACKET params=typedefList RBRACKET { Parameterized(e, params) }
typedefList
  : typedef { [$1] }
  | td=typedef COMMA rest=typedefList { td::rest }

member
: base=expr_atom DOT member=IDENTIFIER
  { { base=base; memberName=member; basetype=Type ""; memberIndex=0} }

tuple_def
: TYPE name=IDENTIFIER EQ LBRACE fields=separated_list(COMMA, tuple_field) RBRACE
  { {name=name;fields=fields} }

tuple_field
  : IDENTIFIER COLON typedef { ($1, $3) }
