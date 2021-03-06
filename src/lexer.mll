{
open Lexing

type lexContext = {
  mutable indents: int list;
  mutable tokenqueue: Grammar.token list;
  mutable nestlevel: int;
}

let nest ctx = ctx.nestlevel <- ctx.nestlevel + 1
let unnest ctx = ctx.nestlevel <- ctx.nestlevel - 1

let parse_operator tok =
  match tok with
  | "+" | "-" -> Grammar.OPERADD(tok)
  | "*" | "/" | "%" -> Grammar.OPERMUL(tok)
  | "=" | "<" | ">" | "!=" | "<=" | ">=" -> Grammar.OPERCMP(tok)
  | _ -> Grammar.OPERATOR(tok)
}
rule coral_token ctx = parse
  | ' ' { coral_token ctx lexbuf }
  | "func" { [Grammar.FUNC] }
  | "if" { [Grammar.IF] }
  | "type" { [Grammar.TYPE] }
  | "let" { [Grammar.LET] }
  | "set" { [Grammar.SET] }
  | "else" { [Grammar.ELSE] }
  | "return" { [Grammar.RETURN] }
  | "=" { [Grammar.EQ] }
  | '-'? '0' 'x' ['0'-'9']+ as tok { [Grammar.INTEGER(tok)] }
  | '-'? ['0'-'9']+ as tok { [Grammar.INTEGER(tok)] }
  | '-'? ['0'-'9']+ '.' ['0'-'9']+ as tok { [Grammar.FLOAT(tok)] }
  | '"' { [string_token (Buffer.create 1024) lexbuf] }
  | '.' { [Grammar.DOT] }
  | '(' { nest ctx; [Grammar.LPAREN] }
  | ')' { unnest ctx; [Grammar.RPAREN] }
  | '{' { nest ctx; [Grammar.LBRACE] }
  | '}' { unnest ctx; [Grammar.RBRACE] }
  | '[' { nest ctx; [Grammar.LBRACKET] }
  | ']' { unnest ctx; [Grammar.RBRACKET] }
  | ',' { [Grammar.COMMA] }
  | ':' { [Grammar.COLON] }
  | '#' { comment ctx lexbuf }
  | '\n' [' ']* as tok {
     let indent_len = String.length tok - 1 in
     lexbuf.lex_curr_p <- {
        lexbuf.lex_curr_p with
        pos_bol = lexbuf.lex_start_pos + 1;
        pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1
     };
     let indents = match ctx.nestlevel, ctx.indents with
       | 0, n :: xs when n < indent_len ->
           ctx.indents <- indent_len :: ctx.indents;
           [Grammar.INDENT]
       | 0, n :: xs when n > indent_len ->
           let rec pop dedents =
              match ctx.indents with
              | n :: xs when n > indent_len ->
                  ctx.indents <- xs;
                  pop (Grammar.DEDENT :: dedents)
              | _ -> dedents
           in pop []
       | _ -> [] in
     (Grammar.NEWLINE(String.length tok) :: indents)
  }
  | ['-' '+' '*' '/' '=' '>' '<' '$' '!' '|' '^' '%' '@']+ as tok { [parse_operator(tok)] }
  | "..." { [Grammar.ELLIPSIS] }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as tok { [Grammar.IDENTIFIER(tok)] }
  | _ as tok { [Grammar.OTHER(tok)] }
  | eof { [Grammar.EOF] }
and comment ctx = parse
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
 | _ { comment ctx lexbuf }
and string_token buf = parse
 | '"' { Grammar.STRING(Buffer.contents buf) }
 | "\\\"" { Buffer.add_char buf '\"'; string_token buf lexbuf }
 | "\\n" { Buffer.add_char buf '\n'; string_token buf lexbuf }
 | "\\t" { Buffer.add_char buf '\t'; string_token buf lexbuf }
 | "\\x" ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']  as tok
   { let num = String.sub tok 2 2 in
     let n = (Scanf.sscanf num "%x" (fun x -> x)) in
     Printf.printf "num: %s n: %d\n" num n;
     Buffer.add_char buf (Char.chr n);
     string_token buf lexbuf }
 | _ as c { Buffer.add_char buf c; string_token buf lexbuf }
