type token =
  | T_INC
  | T_DEC
  | T_PLUS
  | T_MINUS
  | T_DOT
  | T_COMMA
  | T_LBRAC
  | T_RBRAC
  | T_EOF

let rec parse_lines input_line line_num pos =
  let error () =
    failwith
      ("Unexpected character: '"
      ^ String.make 1 input_line.[pos]
      ^ "' at line " ^ Int.to_string line_num ^ ":" ^ Int.to_string pos)
  in
  if pos = String.length input_line then []
  else
    match input_line.[pos] with
    | '>' -> T_INC :: parse_lines input_line line_num (pos + 1)
    | '<' -> T_DEC :: parse_lines input_line line_num (pos + 1)
    | '+' -> T_PLUS :: parse_lines input_line line_num (pos + 1)
    | '-' -> T_MINUS :: parse_lines input_line line_num (pos + 1)
    | '.' -> T_DOT :: parse_lines input_line line_num (pos + 1)
    | ',' -> T_COMMA :: parse_lines input_line line_num (pos + 1)
    | '[' -> T_LBRAC :: parse_lines input_line line_num (pos + 1)
    | ']' -> T_RBRAC :: parse_lines input_line line_num (pos + 1)
    | ' ' -> parse_lines input_line line_num (pos + 1)
    | '/' ->
        if pos + 1 < String.length input_line && input_line.[pos + 1] = '/' then
          []
        else error ()
    | _ -> error ()

let parse str_or_file =
  if Sys.file_exists str_or_file then
    let lines = In_channel.with_open_text str_or_file In_channel.input_lines in
    List.flatten (List.mapi (fun i line -> parse_lines line i 0) lines)
  else parse_lines str_or_file 0 0

let token_to_string = function
  | T_PLUS -> "T_PLUS"
  | T_MINUS -> "T_MINUS"
  | T_INC -> "T_INC"
  | T_DEC -> "T_DEC"
  | T_LBRAC -> "T_LBRAC"
  | T_RBRAC -> "T_RBRAC"
  | T_DOT -> "T_DOT"
  | T_COMMA -> "T_COMMA"
  | T_EOF -> "T_EOF"
