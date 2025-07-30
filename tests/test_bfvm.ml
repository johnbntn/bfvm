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
  let _ = compile "inputs/hello_world.bf" output_file Exe O1 in
  let out = In_channel.input_all (Unix.open_process_args_in output_file [||]) in
  let expected = "Hello World!\n" in
  let _ = Sys.command ("rm " ^ output_file) in
  check string "Not same strings" expected out

let test_optimization () =
  let open Bfvm.Compiler in
  let output_file_opt = "/tmp/opt.ir" in
  let output_file_unopt = "/tmp/unopt.ir" in
  let _ = compile "inputs/opt.bf" output_file_opt IR O1 in
  let _ = compile "inputs/opt.bf" output_file_unopt IR O0 in
  let out_opt = In_channel.open_text output_file_opt in
  let out_unopt = In_channel.open_text output_file_unopt in
  let res = In_channel.length out_opt < In_channel.length out_unopt in
  let expected = true in
  check bool "Unoptimized file is smaller" expected res

let suite =
  [
    ("Parsing", `Quick, test_parser);
    ("Codegen", `Quick, test_codegen);
    ("Optimization", `Quick, test_optimization);
  ]

let () = Alcotest.run "BFVM" [ ("Test", suite) ]
