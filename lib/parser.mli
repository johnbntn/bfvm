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

val parse : string -> token list
val token_to_string : token -> string
