%{
open Ast
%}

%token <int> INTEGER
%token <string> OPERATOR IDENTIFIER STRING
%token <char> OTHER
%token FUNC IF ELSE COMMA ELLIPSIS
%token LPAREN RPAREN COLON INDENT DEDENT
%token <int> NEWLINE
(*
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
*)
%start main
%token EOF
%type <Ast.node> expr line block main
%type <Ast.node list> lines
%type <Ast.defNode list> paramlist
%%

main
  : x=lines EOF        { Module(x) }
  | x=lines e=expr EOF { Module(x @ [e]) }

line
  : FUNC name=IDENTIFIER LPAREN p=paramlist RPAREN body=block { Func(name, Free, p, body) }
  | IF cond=expr ifbody=block { If(cond, ifbody, Empty) }
  | IF cond=expr ifbody=block ELSE elsebody=block { If(cond, ifbody, elsebody) }
  | e=expr NEWLINE { e }

lines
  : e=line { [e] }
  | x=lines e=line { x @ [e] }
  | x=lines NEWLINE { x }

block
  : COLON NEWLINE INDENT lines=lines DEDENT { Block(lines) }

expr
  : e=INTEGER { IntLiteral (string_of_int e) }
  | e=IDENTIFIER { Var {name=e; target=None} }
  | e=STRING { StringLiteral e }
  | lhs=expr op=OPERATOR rhs=expr { Binop (op, lhs, rhs) }
  | callee=expr LPAREN args=exprlist RPAREN { Call(callee, args) }
  | callee=expr arg=expr { Call(callee, [arg]) }

exprlist
  : e=expr { [e] }
  | x=exprlist COMMA e=expr { x@[e] }

paramlist
  : e=IDENTIFIER { [{name=e; defType=None}] }
  | x=paramlist COMMA e=IDENTIFIER { x@[{name=e; defType=None}] }
