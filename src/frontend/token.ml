type token =
  | TYPE
  | STRING of string
  | SET
  | RPAREN
  | RETURN
  | YIELD
  | RBRACKET
  | RBRACE
  | OTHER of char
  | CHAR of char
  | OPERMUL of string
  | OPERCMP of string
  | OPERATOR of string
  | OPERADD of string
  | NEWLINE of int
  | IMPORT
  | EXTERN
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
  | ATTRIBUTE_NAME of string (* @identifier *)
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
