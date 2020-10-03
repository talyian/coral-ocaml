type token =
  | TYPE
  | STRING of string
  | SET
  | RPAREN
  | RETURN
  | RBRACKET
  | RBRACE
  | OTHER of char
  | CHAR of char
  | OPERMUL of string
  | OPERCMP of string
  | OPERATOR of string
  | OPERADD of string
  | NEWLINE of int
  | LPAREN
  | LET
  | LBRACKET
  | LBRACE
  | INTEGER of string
  | INDENT
  | IF
  | ELIF
  | FOR
  | IN
  | IDENTIFIER of string
  | FUNC
  | FLOAT of string
  | EQ
  | EOF
  | ELSE
  | ELLIPSIS
  | DOT
  | DEDENT
  | COMMA
  | COLON
  | SEMICOLON
[@@deriving show]
