let rec get_opcodes the_module curr_instr =
  match curr_instr with
  | None -> print_endline "at end of block"
  | Some instr ->
    let get_next instr =
      match Llvm.instr_succ instr with
      | At_end _ -> None
      | Before instr -> Some instr
    in
    let op = Llvm.instr_opcode instr in
    let open Llvm.Opcode in
    match op with
    | Load ->
      let _ = print_endline "got a load" in
      let next_instr = get_next instr in
      get_opcodes the_module next_instr
    | Add ->
      let _ = print_endline "got an add" in
      let next_instr = get_next instr in
      get_opcodes the_module next_instr
    | Sub ->
      let _ = print_endline "got a sub" in
      let next_instr = get_next instr in
      get_opcodes the_module next_instr
    | Store ->
      let _ = print_endline "got a store" in
      let next_instr = get_next instr in
      get_opcodes the_module next_instr
    | _ ->
      let _ = print_endline "Not yet doing that" in
      let next_instr = get_next instr in
      get_opcodes the_module next_instr

let fold_global_ops the_module =
  Llvm.iter_functions (fun fn ->
    Llvm.iter_blocks (fun blk ->
      let first_instr = match Llvm.instr_begin blk with
      | At_end _ -> None
      | Before instr -> Some instr
      in
      let _ = get_opcodes the_module first_instr in
      ()
    ) fn
  ) the_module