open Alcotest

let list_to_str f tokens = String.concat "--" (List.map f tokens)

let test_parser () =
  let open Bfvm.Parser in
  let tokens = parse "inputs/ops.bf" in
  let tokens_str = list_to_str token_to_string tokens in
  let expected =
    "T_PLUS--T_MINUS--T_DEC--T_INC--T_DOT--" ^ "T_COMMA--T_LBRAC--T_RBRAC"
  in
  check string "Not same strings" expected tokens_str

let test_codegen () =
  let open Bfvm.Compiler in
  let output_file = "/tmp/hello_test.out" in
  let _ = compile "inputs/hello_world.bf" output_file Exe in
  let out = In_channel.input_all (Unix.open_process_args_in output_file [||]) in
  let expected = "Hello World!\n" in
  let _ = Sys.command ("rm " ^ output_file) in
  check string "Not same strings" expected out

let suite =
  [ ("Parsing", `Quick, test_parser); ("Codegen", `Quick, test_codegen) ]

let () = Alcotest.run "BFVM" [ ("Test", suite) ]
