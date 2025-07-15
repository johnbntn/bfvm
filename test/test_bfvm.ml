open Alcotest
open Parse

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

let list_to_str f tokens = String.concat "--" (List.map f tokens)
let test_simple () = 
  let tokens = parse "inputs/simple.bf" in
  let tokens_str = list_to_str token_to_string tokens in
  let expected = "T_PLUS--T_MINUS--T_DEC--T_INC--T_DOT--" ^
                    "T_COMMA--T_LBRAC--T_RBRAC--T_EOF" in
  check string "same list" expected tokens_str

let suite =
[
  "Simple parsing test", `Quick, test_simple
]

let () =
  Alcotest.run "BFVM" [ "Parsing", suite ]