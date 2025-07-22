let _ =
  let open Bfvm in
  let _ = Llvm_all_backends.initialize () in
  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let cpu = "generic" in
  let reloc_mode = Llvm_target.RelocMode.Default in
  let machine = Llvm_target.TargetMachine.create ~triple:target_triple ~cpu ~reloc_mode target in
  let data_layout = Llvm_target.TargetMachine.data_layout machine |> Llvm_target.DataLayout.as_string in

  let in_file = Sys.argv.(1) in
  let tokens = Parser.parse in_file in
  let the_module = Codegen.generate tokens in

  Llvm.set_target_triple target_triple the_module;
  Llvm.set_data_layout data_layout the_module;
  let out_file = "output.o" in
  let file_type = Llvm_target.CodeGenFileType.ObjectFile in
  let _ = Llvm_target.TargetMachine.emit_to_file the_module file_type out_file machine in
  let _ = Llvm.dump_module the_module in
  ()