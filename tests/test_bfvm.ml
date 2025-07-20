open Alcotest
open Bfvm.Parser

let list_to_str f tokens = String.concat "--" (List.map f tokens)
let test_simple () = 
  let tokens = parse "inputs/ops.bf" in
  let tokens_str = list_to_str token_to_string tokens in
  let expected = "T_PLUS--T_MINUS--T_DEC--T_INC--T_DOT--" ^
                    "T_COMMA--T_LBRAC--T_RBRAC" in
  check string "Not same strings" expected tokens_str

let suite =
[
  "Simple parsing test", `Quick, test_simple
]

let () =
  Alcotest.run "BFVM" [ "Parsing", suite ]