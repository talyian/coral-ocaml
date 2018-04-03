%token <int> INTEGER
%token <string> OPERATOR IDENTIFIER STRING
%token <char> OTHER
%token FUNC IF ELSE WHILE LET SET
%token ELLIPSIS PLUS MINUS TIMES DIV
%token LPAREN RPAREN COLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%start main
%token EOF
%type <int> main

%%

main
  : e = line EOF                { 0 }
  ;

line
  : FUNC IDENTIFIER LPAREN IDENTIFIER RPAREN COLON { }
;
expr
  : n=INTEGER                 { n }
  | LPAREN e = expr RPAREN  { e }
  | e1=expr PLUS  e2=expr   { e1 + e2 }
  | e1=expr MINUS e2=expr   { e1 - e2 }
  | e1=expr TIMES e2=expr   { e1 * e2 }
  | e1=expr DIV   e2=expr   { e2 / e1 }
  | MINUS e = expr %prec UMINUS { - e }
;
