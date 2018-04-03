{
}
rule coral_token = parse
  | ' ' { coral_token lexbuf }
  | "func" { Grammar.FUNC }
  | "if" { Grammar.IF }
  | "else" { Grammar.ELSE }
  | ['0'-'9']+ as tok { Grammar.INTEGER(int_of_string tok) }
  | '"' { string_token (Buffer.create 1024) lexbuf }
  | ['-' '+' '*' '/' '=' '>' '<' '$' '!' '|' '^' '%' '@']+ as tok { Grammar.OPERATOR(tok) }
  | '.' '.' '.' { Grammar.ELLIPSIS }
  | _ as tok { Grammar.OTHER(tok) }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as tok { Grammar.IDENTIFIER(tok) }
  | eof { EOF }
and string_token buf = parse
 | '"' { Grammar.STRING(Buffer.contents buf) }
 | _ as c { Buffer.add_char buf c; string_token buf lexbuf }
