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
%token LPAREN RPAREN COLON INDENT DEDENT
%token EQ

(*
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
*)
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
  : FUNC name=IDENTIFIER LPAREN p=paramlist RPAREN body=block
    { Func(newFunc (name, Type(""), p, body)) }
  | FUNC name=IDENTIFIER LPAREN p=paramlist RPAREN NEWLINE
    { Func(newFunc (name, Type(""), p, Empty)) }
  | FUNC name=IDENTIFIER COLON ret=typedef LPAREN  p=paramlist RPAREN body=block
    { Func(newFunc (name, ret, p, body)) }
  | FUNC name=IDENTIFIER COLON ret=typedef LPAREN  p=paramlist RPAREN NEWLINE
    { Func(newFunc (name, ret, p, Empty)) }
  | LET name=IDENTIFIER EQ e=expr NEWLINE { Let({name=name;target=None;varType=None}, e) }
  | LET name=IDENTIFIER COLON t=typedef EQ e=expr NEWLINE {
      Let({name=name;target=None;varType=Some(t)}, e) }
  | SET name=IDENTIFIER EQ e=expr NEWLINE { Set({name=name;target=None;varType=None}, e) }
  | RETURN arg=expr NEWLINE { Return {node=arg;coraltype=None} }
  | RETURN NEWLINE { Return {node=Tuple [];coraltype=None} }
  | e=expr NEWLINE { e }
  | e=ifexpr {e}

ifexpr
  : IF cond=expr2 ifbody=block_or_line { If(cond, ifbody, Empty) }
  | IF cond=expr2 ifbody=block_or_line ELSE elsebody=block_or_line { If(cond, ifbody, elsebody) }
  | IF cond=expr2 ifbody=block_or_line ELSE elsebody=ifexpr { If(cond, ifbody, elsebody) }

lines
  : line { [$1] }
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
  | e=STRING { StringLiteral e }
  | LPAREN e=expr2 RPAREN { e }

(* Higher Precedence than operators *)
expr_op_unit
  : e=expr_atom { e }
  | callee=expr_atom LPAREN args=exprlist RPAREN { Call(callee, args) }
  | callee=expr_op_unit LPAREN RPAREN { Call(callee, []) }
  | callee=expr_op_unit arg=expr_atom { Call(callee, [arg]) }

expr0
  : e=expr_op_unit { e }
  | lhs=expr0 op=OPERMUL rhs=expr0 { Binop(op, lhs, rhs) }

expr1
  : e=expr0 { e }
  | lhs=expr1 op=OPERADD rhs=expr1 { Binop(op, lhs, rhs) }
  | lhs=expr1 op=OPERATOR rhs=expr1 { Binop(op, lhs, rhs) }

expr2
  : e=expr1 { e }
  | l=expr2 o=OPERCMP r=expr2 { Binop (o, l, r) }
  | l=expr2 EQ r=expr2 { Binop ("=", l, r) }

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
  : p=param { [p] }
  | x=paramlist COMMA p=param { x@[p] }

typedef
  : e=IDENTIFIER { Type e }
  | ELLIPSIS { Type "..." }
