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

let rec lex input pos = 
  if pos >= String.length input then [T_EOF]
  else
    match input.[pos] with
    | '>' -> T_INC :: lex input (pos + 1)
    | '<' -> T_DEC :: lex input (pos + 1)
    | '+' -> T_PLUS :: lex input (pos + 1)
    | '-' -> T_MINUS :: lex input (pos + 1)
    | '.' -> T_DOT :: lex input (pos + 1)
    | ',' -> T_COMMA :: lex input (pos + 1)
    | '[' -> T_LBRAC :: lex input (pos + 1)
    | ']' -> T_RBRAC :: lex input (pos + 1)
    | _ -> failwith ("Unexpected character: " ^ String.make 1 input.[pos])