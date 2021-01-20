type parseError =
  { file_name: string option
  ; error_token: string
  ; context_before: string
  ; context_after: string
  ; column_number: int
  ; line_number: int }

let show_parseError e =
  let open Printf in
  (match e.file_name with Some f -> sprintf " (%s) " f | None -> "")
  ^ sprintf "Parse Error: Line %d:%d: [" e.line_number e.column_number
  ^ sprintf "%s\x1b[1;41m%s\x1b[0m%s\n%s%s\n" e.context_before e.error_token e.context_after
      (if e.column_number < 0 then "" else String.make e.column_number ' ')
      (String.make (String.length e.error_token) '^')
  ^ "]"

let parse_string ?file_name s : (Coral_core.Ast.t, parseError) Result.t =
  let lexbuf = Lexing.from_string s in
  let tokenf = LexerInterface.create_tokenizer () in
  match Grammar.main tokenf lexbuf with
  | Main expr -> Ok expr
  | exception _ ->
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = Lexing.lexeme_end_p lexbuf in
      let lexeme = Lexing.lexeme lexbuf in
      let error_start = startp.pos_cnum in
      let error_end = endp.pos_cnum in
      let context_start =
        let rec f n pos =
          if pos <= 0 then 0
          else if pos >= String.length s then String.length s
          else if s.[pos] = '\n' then if n = 0 then pos else f (n - 1) (pos - 1)
          else f n (pos - 1) in
        f 2 error_start in
      let context_end =
        let rec f pos mpos =
          if pos = mpos then pos else if s.[pos] = '\n' then pos else f (pos + 1) mpos in
        f error_end (String.length s) in
      Error
        { file_name
        ; error_token= lexeme
        ; column_number= startp.pos_cnum - startp.pos_bol
        ; line_number= startp.pos_lnum
        ; context_before= String.sub s context_start (error_start - context_start)
        ; context_after= String.sub s error_end (context_end - error_end) }
