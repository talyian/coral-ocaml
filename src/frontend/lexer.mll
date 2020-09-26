{
open Lexing

type lexContext = {
  mutable indents: int list;
  mutable tokenqueue: Token.token list;
  mutable nestlevel: int;
}

let nest ctx = ctx.nestlevel <- ctx.nestlevel + 1
let unnest ctx = ctx.nestlevel <- ctx.nestlevel - 1

(* TODO: A push parser should make this cleaner since we can just push tokens as
needed instead of maintaining a mutable token queue to handle generating
multiple tokens per lexing rule. *)
let handle_newline indent_len ctx lexbuf =
     lexbuf.lex_curr_p <- {
        lexbuf.lex_curr_p with
        pos_bol = lexbuf.lex_start_pos + 1;
        pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
     };
     let nl = Token.NEWLINE indent_len in      
     let indents = match ctx.nestlevel, ctx.indents with
       | 0, n :: _ when n < indent_len ->
           ctx.indents <- indent_len :: ctx.indents;
           [nl;Token.INDENT]
       | 0, n :: _ when n > indent_len ->
           let rec pop dedents =
              match ctx.indents with
              | n :: xs when n > indent_len ->
                  ctx.indents <- xs;
                  pop (Token.DEDENT :: Token.NEWLINE n :: dedents)
              | _ -> dedents
           in nl :: pop []
       | _ -> [nl] in
       indents  

(* If we parse operators are separate token types, we can use different
production rules in the grammar to handle precedence. TODO-wish: Instead of
embedding this heirarchy in the grammar productions, we should use a single
"binop : expr OPERATOR expr" production and use Pratt-style operator precedence
table; this will allow for custom operators. Instead of a strict numeric rating
though the precedence should be a set of pairs of "A binds over B" *)
let parse_operator tok =
  match tok with
  | "+" | "-" -> Token.OPERADD(tok)
  | "*" | "/" | "%" -> Token.OPERMUL(tok)
  | "=" | "<" | ">" | "!=" | "<=" | ">=" -> Token.OPERCMP(tok)
  | _ -> Token.OPERATOR(tok)
}
rule coral_token ctx = parse
  | ' ' { coral_token ctx lexbuf }
  | "func" { [Token.FUNC] }
  | "return" { [Token.RETURN] }

  | "if" { [Token.IF] }
  | "else" { [Token.ELSE] }
  | "elif" { [Token.ELIF] }
  | "for" { [Token.FOR] }
  | "in"  { [Token.IN] }
  
  | "type" { [Token.TYPE] }
  | "let" { [Token.LET] }
  | "set" { [Token.SET] }

  | "=" { [Token.EQ] }
  | '-'? '0' 'x' ['0'-'9']+ as tok { [Token.INTEGER(tok)] }
  | '-'? ['0'-'9']+ as tok { [Token.INTEGER(tok)] }
  | '-'? ['0'-'9']+ '.' ['0'-'9']+ as tok { [Token.FLOAT(tok)] }
  | '"' { [string_token (Buffer.create 1024) lexbuf] }
  | '.' { [Token.DOT] }
  | '(' { nest ctx; [Token.LPAREN] }
  | ')' { unnest ctx; [Token.RPAREN] }
  | '{' { nest ctx; [Token.LBRACE] }
  | '}' { unnest ctx; [Token.RBRACE] }
  | '[' { nest ctx; [Token.LBRACKET] }
  | ']' { unnest ctx; [Token.RBRACKET] }
  | ',' { [Token.COMMA] }
  | ':' { [Token.COLON] }
  | '#' { comment ctx lexbuf }
  | '\n' [' ']* as tok { handle_newline (String.length tok - 1) ctx lexbuf }
  | ['-' '+' '*' '/' '=' '>' '<' '$' '!' '|' '^' '%' '@']+ as tok { [parse_operator(tok)] }
  | "..." { [Token.ELLIPSIS] }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as tok { [Token.IDENTIFIER(tok)] }
  | _ as tok { [Token.OTHER(tok)] }
  | eof { handle_newline 0 ctx lexbuf @ [Token.EOF] }
and comment ctx = parse
  | '\n' [' ']* as tok { handle_newline (String.length tok - 1) ctx lexbuf }
  | _ { comment ctx lexbuf }
and string_token buf = parse
  | '"' { Token.STRING(Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; string_token buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; string_token buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; string_token buf lexbuf }
  | "\\x" ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']  as tok
    { let num = String.sub tok 2 2 in
     let n = (Scanf.sscanf num "%x" (fun x -> x)) in
     Printf.printf "num: %s n: %d\n" num n;
     Buffer.add_char buf (Char.chr n);
     string_token buf lexbuf }
  | eof { failwith "unterminated string" }
  | '\\' { failwith "unrecognized escape" } 
  | _ as c { Buffer.add_char buf c; string_token buf lexbuf }