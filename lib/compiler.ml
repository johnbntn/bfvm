open Cmdliner

type opt_level = O0 | O1
type file_type = Object | Asm | IR | Exe

let output_file_type =
  let default = Exe in
  let object_flag =
    let info = Arg.info [ "c" ] ~doc:"Output object file" in
    (Object, info)
  in
  let asm_flag =
    let info = Arg.info [ "S" ] ~doc:"Output assembly file" in
    (Asm, info)
  in
  let ir_flag =
    let info = Arg.info [ "i" ] ~doc:"Output LLVM IR file" in
    (IR, info)
  in
  Arg.value (Arg.vflag default [ object_flag; asm_flag; ir_flag ])

let input_file =
  let info = Arg.info [] ~docv:"FILE" ~doc:"Input BF file" in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let output_file =
  let default = "a.out" in
  let info =
    Arg.info [ "o"; "output" ] ~docv:"FILE"
      ~doc:"Output file, defaults to 'a.out'"
  in
  Arg.value (Arg.opt Arg.string default info)

let opt_level_conv =
  let parse s =
    match s with
    | "0" -> Ok O0
    | "1" -> Ok O1
    | _ -> Error (`Msg "optimization level must be 0, 1")
  in
  let print ppf = function
    | O0 -> Format.fprintf ppf "0"
    | O1 -> Format.fprintf ppf "1"
  in
  Arg.conv (parse, print)

let opt_level =
  let default = O1 in
  let info = Arg.info [ "O" ] ~doc:"Optimization Level {0, 1}" in
  Arg.value (Arg.opt opt_level_conv default info)

let compile in_file out_file out_file_type opt_level =
  let _ = Llvm_all_backends.initialize () in
  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let cpu = "generic" in
  let reloc_mode = Llvm_target.RelocMode.Default in
  let machine =
    Llvm_target.TargetMachine.create ~triple:target_triple ~cpu ~reloc_mode
      target
  in
  let data_layout =
    Llvm_target.TargetMachine.data_layout machine
    |> Llvm_target.DataLayout.as_string
  in

  let tokens = Parser.parse in_file in
  let the_module = Codegen.generate tokens in

  let _ =
    match opt_level with O0 -> () | O1 -> Passes.fold_loops the_module
  in

  Llvm.set_target_triple target_triple the_module;
  Llvm.set_data_layout data_layout the_module;

  (* Generate different file formats depending on flags *)
  match out_file_type with
  | Object ->
      let file_type = Llvm_target.CodeGenFileType.ObjectFile in
      Llvm_target.TargetMachine.emit_to_file the_module file_type out_file
        machine
  | Exe ->
      let file_type = Llvm_target.CodeGenFileType.ObjectFile in
      let obj = "temporary_object_file.o" in
      let _ =
        Llvm_target.TargetMachine.emit_to_file the_module file_type obj machine
      in
      let _ = Sys.command ("clang temporary_object_file.o -o " ^ out_file) in
      let _ = Sys.command "rm temporary_object_file.o" in
      ()
  | Asm ->
      let file_type = Llvm_target.CodeGenFileType.AssemblyFile in
      Llvm_target.TargetMachine.emit_to_file the_module file_type out_file
        machine
  | IR -> Llvm.print_module out_file the_module

let run_compiler () =
  let compiler_term =
    Term.(
      const compile $ input_file $ output_file $ output_file_type $ opt_level)
  in
  let compiler_cmd =
    let program =
      Cmd.info "bfc" ~doc:"A compiler for the Brainf*ck programming language"
    in
    Cmd.v program compiler_term
  in
  exit (Cmd.eval compiler_cmd)
