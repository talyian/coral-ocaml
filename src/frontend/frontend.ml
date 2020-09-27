
type parseError = {
  error_token: string;
  context_before: string;
  context_after: string;
  column_number: int;
  line_number: int;
}
let show_parseError e =
  Printf.sprintf "%s\x1b[1;41m%s\x1b[0m%s\n%s%s\n"
    e.context_before
    e.error_token
    e.context_after
    (String.make e.column_number ' ')
    (String.make (String.length e.error_token) '^')
;;

let parse_string s =
  let lexbuf = Lexing.from_string s in
  let tokenf = LexerInterface.create_tokenizer () in
  try
    Ok(Grammar.main tokenf lexbuf)
  with
  | Grammar.Error ->
    let startp = Lexing.lexeme_start_p lexbuf in
    let endp = Lexing.lexeme_end_p lexbuf in
    let lexeme = Lexing.lexeme lexbuf in
    let e_start = startp.pos_cnum in
    let e_end = endp.pos_cnum in
    let c_start =
      let rec f n pos =
        if pos = 0 then 0
        else if String.get s pos = '\n' then
          if n = 0 then pos
          else f (n - 1) (pos - 1)
        else f n (pos - 1) in f 2 e_start in
    let c_end =
      let rec f pos mpos =
        if pos = mpos then pos
        else if String.get s pos = '\n' then pos
        else f (pos + 1) mpos in f e_end (String.length s) in
    Error {
      error_token=lexeme;
      column_number = startp.pos_cnum - startp.pos_bol;
      line_number = startp.pos_lnum;
      context_before = String.sub s c_start (e_start - c_start);
      context_after = String.sub s e_end (c_end - e_end);
    }
