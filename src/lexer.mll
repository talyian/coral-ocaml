{
open Lexing

type lexContext = {
  mutable indents: int list;
  mutable tokenqueue: Grammar.token list;
}

}
rule coral_token ctx = parse
  | ' ' { coral_token ctx lexbuf }
  | "func" { [Grammar.FUNC] }
  | "if" { [Grammar.IF] }
  | "else" { [Grammar.ELSE] }
  | ['0'-'9']+ as tok { [Grammar.INTEGER(int_of_string tok)] }
  | '"' { [string_token (Buffer.create 1024) lexbuf] }
  | '(' { [Grammar.LPAREN] }
  | ')' { [Grammar.RPAREN] }
  | ',' { [Grammar.COMMA] }
  | ':' { [Grammar.COLON] }
  | '\n' [' ']* as tok {
     let indent_len = String.length tok - 1 in
     lexbuf.lex_curr_p <- {
        lexbuf.lex_curr_p with
        pos_bol = indent_len;
        pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1
     };
     (Grammar.NEWLINE(String.length tok) :: (match ctx.indents with
       | n :: xs when n < indent_len ->
           ctx.indents <- indent_len :: ctx.indents;
           [Grammar.INDENT]
       | n :: xs when n > indent_len ->
           let rec pop dedents =
              match ctx.indents with
              | n :: xs when n > indent_len ->
                  ctx.indents <- xs;
                  pop (Grammar.DEDENT :: dedents)
              | _ -> dedents
           in pop []
       | _ -> []
     ))
  }
  | ['-' '+' '*' '/' '=' '>' '<' '$' '!' '|' '^' '%' '@']+ as tok { [Grammar.OPERATOR(tok)] }
  | '.' '.' '.' { [Grammar.ELLIPSIS] }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as tok { [Grammar.IDENTIFIER(tok)] }
  | _ as tok { [Grammar.OTHER(tok)] }
  | eof { [Grammar.EOF] }
and string_token buf = parse
 | '"' { Grammar.STRING(Buffer.contents buf) }
 | _ as c { Buffer.add_char buf c; string_token buf lexbuf }
