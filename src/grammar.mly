%{
open Ast
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> OPERATOR IDENTIFIER STRING
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
  : x=lines EOF        { Module(x) }
  | x=lines e=expr EOF { Module(x @ [e]) }

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
  | e=expr NEWLINE { e }
  | e=ifexpr {e}

ifexpr
  : IF cond=expr ifbody=block_or_line { If(cond, ifbody, Empty) }
  | IF cond=expr ifbody=block_or_line ELSE elsebody=block_or_line { If(cond, ifbody, elsebody) }
  | IF cond=expr ifbody=block_or_line ELSE elsebody=ifexpr { If(cond, ifbody, elsebody) }

lines
  : e=line { [e] }
  | x=lines e=line { x @ [e] }
  | x=lines NEWLINE { x }

block
  : COLON NEWLINE INDENT lines=lines DEDENT { Block(lines) }
block_or_line
  : e=block { e }
  | COLON e=expr NEWLINE { e }

expr
  : e=INTEGER { IntLiteral e }
  | e=FLOAT { FloatLiteral e }
  | e=IDENTIFIER { Var {name=e; target=None; varType=None} }
  | e=STRING { StringLiteral e }
  | lhs=expr op=operator rhs=expr { Binop (op, lhs, rhs) }
  | callee=expr LPAREN args=exprlist RPAREN { Call(callee, args) }
  | callee=expr LPAREN RPAREN { Call(callee, []) }
  | callee=expr arg=expr { Call(callee, [arg]) }
  | RETURN arg=expr { Return {node=arg;coraltype=None} }
  | RETURN { Return {node=Tuple [];coraltype=None} }
exprlist
  : e=expr { [e] }
  | x=exprlist COMMA e=expr { x@[e] }

operator
  : e=OPERATOR { e }
  | EQ { "=" }

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
