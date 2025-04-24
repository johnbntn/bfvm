(*
- take input from file
- put it into array
- scan over array and make sure all are valid tokens
- scan over again and make sure order is correct
*)

(* exposing type for testing *)
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

val lex : string -> int -> token list