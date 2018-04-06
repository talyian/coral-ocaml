%{
open Ast
%}

%token <string> INTEGER
%token <string> FLOAT
%token <string> OPERATOR IDENTIFIER STRING
%token <int> NEWLINE
%token <char> OTHER
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
%type <Ast.defNode list> paramlist
%%

main
  : x=lines EOF        { Module(x) }
  | x=lines e=expr EOF { Module(x @ [e]) }

line
  : FUNC name=IDENTIFIER LPAREN p=paramlist RPAREN body=block
    { Func(name, Type("void"), p, body) }
  | FUNC name=IDENTIFIER LPAREN p=paramlist RPAREN NEWLINE
    { Func(name, Type("void"), p, Empty) }
  | FUNC name=IDENTIFIER COLON ret=typedef LPAREN  p=paramlist RPAREN body=block
    { Func(name, ret, p, body) }
  | FUNC name=IDENTIFIER COLON ret=typedef LPAREN  p=paramlist RPAREN NEWLINE
    { Func(name, ret, p, Empty) }
  | LET name=IDENTIFIER EQ e=expr NEWLINE { Let({name=name;target=None}, e) }
  | SET name=IDENTIFIER EQ e=expr NEWLINE { Set({name=name;target=None}, e) }
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
  | e=IDENTIFIER { Var {name=e; target=None} }
  | e=STRING { StringLiteral e }
  | lhs=expr op=OPERATOR rhs=expr { Binop (op, lhs, rhs) }
  | callee=expr LPAREN args=exprlist RPAREN { Call(callee, args) }
  | callee=expr arg=expr { Call(callee, [arg]) }
  | RETURN arg=expr { Return(arg) }
  | RETURN { Return (Tuple []) }
exprlist
  : e=expr { [e] }
  | x=exprlist COMMA e=expr { x@[e] }

param
  : e=IDENTIFIER { {name=e; defType=None} }
  | e=IDENTIFIER COLON t=typedef { {name=e; defType=Some t} }
  | ELLIPSIS { {name="..."; defType=Some (Type "...") } }
paramlist
  : e=non_empty_paramlist { e }
  | { [] }
non_empty_paramlist
  : p=param { [p] }
  | x=paramlist COMMA p=param { x@[p] }

typedef
  : e=IDENTIFIER { Type e }
  | ELLIPSIS { Type "..." }
