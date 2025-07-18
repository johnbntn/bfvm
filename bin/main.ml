let _ =
  let open Bfvm in
  let filename = Sys.argv.(1) in
  let tokens = Parser.parse filename in
  Llvm.dump_module (Codegen.generate tokens)