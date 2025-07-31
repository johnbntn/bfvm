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
(** [parse str] parses string [str] of BF code and returns a list of tokens *)

val token_to_string : token -> string
(** [token_to_string t] returns a string representation of token [t] *)
