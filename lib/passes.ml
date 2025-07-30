type fold_loop_state =
  | Looking
  | FoundLoadPtr
  | FoundGep
  | FoundLoadTape
  | FoundSub
  | FoundStore

let get_bb_succ bb =
  let pos = Llvm.block_succ bb in
  Option.get
    (match pos with Llvm.At_end _ -> None | Llvm.Before bb -> Some bb)

let unwrap_llpos pos =
  Option.get
    (match pos with Llvm.At_end _ -> None | Llvm.Before instr -> Some instr)

let fold the_module bb =
  let the_context = Llvm.module_context the_module in
  let builder = Llvm.builder the_context in

  let i8_type = Llvm.i8_type the_context in
  let const_zero = Llvm.const_int i8_type 0 in

  let header_bb = get_bb_succ bb in

  let ptr_load = unwrap_llpos (Llvm.instr_begin header_bb) in
  let gep = unwrap_llpos (Llvm.instr_succ ptr_load) in
  let cell_load = unwrap_llpos (Llvm.instr_succ gep) in
  let cell_cmp = unwrap_llpos (Llvm.instr_succ cell_load) in
  let cond_br = unwrap_llpos (Llvm.instr_succ cell_cmp) in

  let uncond_br =
    match Llvm.get_branch cond_br with
    | Some (`Conditional (_, _, else_block)) -> else_block
    | _ -> failwith "Expected conditional branch"
  in

  let _ = Llvm.position_before cell_cmp builder in

  let _ = Llvm.build_store const_zero cell_load builder in
  let _ = Llvm.build_br uncond_br builder in

  let _ = Llvm.delete_instruction cell_cmp in
  let _ = Llvm.delete_instruction cond_br in

  let clear_block block =
    let rec clear_instructions () =
      match Llvm.instr_begin block with
      | At_end _ -> ()
      | Before instr ->
          Llvm.delete_instruction instr;
          clear_instructions ()
    in
    clear_instructions ()
  in
  let _ = clear_block bb in
  let _ = Llvm.position_at_end bb builder in
  let _ = Llvm.build_unreachable builder in
  ()

let rec fold_loop_state_machine the_module curr_instr state bb =
  match curr_instr with
  | None -> ()
  | Some instr -> (
      let get_next instr =
        match Llvm.instr_succ instr with
        | At_end _ -> None
        | Before instr -> Some instr
      in
      let op = Llvm.instr_opcode instr in
      let next_instr = get_next instr in
      let open Llvm.Opcode in
      match op with
      | Load -> (
          match state with
          | Looking ->
              fold_loop_state_machine the_module next_instr FoundLoadPtr bb
          | FoundGep ->
              fold_loop_state_machine the_module next_instr FoundLoadTape bb
          | _ -> ())
      | GetElementPtr -> (
          match state with
          | FoundLoadPtr ->
              fold_loop_state_machine the_module next_instr FoundGep bb
          | _ -> ())
      | Sub -> (
          match state with
          | FoundLoadTape ->
              fold_loop_state_machine the_module next_instr FoundSub bb
          | _ -> ())
      | Store -> (
          match state with
          | FoundSub ->
              fold_loop_state_machine the_module next_instr FoundStore bb
          | _ -> ())
      | Br -> ( match state with FoundStore -> fold the_module bb | _ -> ())
      | _ -> ())

let fold_loops the_module =
  Llvm.iter_functions
    (fun fn ->
      Llvm.iter_blocks
        (fun bb ->
          let first_instr =
            match Llvm.instr_begin bb with
            | At_end _ -> None
            | Before instr -> Some instr
          in
          let _ = fold_loop_state_machine the_module first_instr Looking bb in
          ())
        fn)
    the_module
