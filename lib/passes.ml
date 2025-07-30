[@@@warning "-37"]
[@@@warning "-34"]

type fold_global_state =
  | Looking
  | FirstLoad
  | FirstArith
  | FoldingStore
  | FoldingLoad
  | FoldingArith

type fold_loop_state =
  | Looking
  | FoundLoadPtr
  | FoundGep
  | FoundLoadTape
  | FoundSub
  | FoundStore

type fold_loop_ctx = {
  tape_cell : Llvm.llvalue option;
  state : fold_loop_state;
}

(* Only use this when you know the bb exists *)
let get_bb_succ bb =
  let pos = Llvm.block_succ bb in
  Option.get
    (match pos with Llvm.At_end _ -> None | Llvm.Before bb -> Some bb)

[@@@warning "-32"]

let unwrap_llpos pos =
  Option.get
    (match pos with Llvm.At_end _ -> None | Llvm.Before instr -> Some instr)

[@@@warning "-27"]

let fold the_module tape_cell bb =
  let the_context = Llvm.module_context the_module in
  let builder = Llvm.builder the_context in

  let i8_type = Llvm.i8_type the_context in
  let const_zero = Llvm.const_int i8_type 0 in

  let header_bb = get_bb_succ bb in

  let ptr_load = unwrap_llpos (Llvm.instr_begin header_bb) in
  let _ = print_endline ("got ptr load: " ^ Llvm.string_of_llvalue ptr_load) in

  let gep = unwrap_llpos (Llvm.instr_succ ptr_load) in
  let _ = print_endline ("got gep: " ^ Llvm.string_of_llvalue gep) in

  let cell_load = unwrap_llpos (Llvm.instr_succ gep) in
  let _ =
    print_endline ("got cell load: " ^ Llvm.string_of_llvalue cell_load)
  in

  let cell_cmp = unwrap_llpos (Llvm.instr_succ cell_load) in
  let _ = print_endline ("got cell cmp: " ^ Llvm.string_of_llvalue cell_cmp) in

  let cond_br = unwrap_llpos (Llvm.instr_succ cell_cmp) in
  let _ = print_endline ("got cond br: " ^ Llvm.string_of_llvalue cond_br) in

  let uncond_br =
    match Llvm.get_branch cond_br with
    | Some (`Conditional (_, _, else_block)) -> else_block
    | _ -> failwith "Expected conditional branch"
  in

  let _ = Llvm.position_before cell_cmp builder in

  let dbg = Llvm.build_store const_zero cell_load builder in
  let _ = print_endline ("build store: " ^ Llvm.string_of_llvalue dbg) in

  let dbg = Llvm.build_br uncond_br builder in
  let _ = print_endline ("build branch: " ^ Llvm.string_of_llvalue dbg) in

  let _ = Llvm.delete_instruction cell_cmp in
  let _ = print_endline "first" in
  let _ = Llvm.delete_instruction cond_br in
  let _ = print_endline "second" in

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

let rec fold_loop_state_machine the_module curr_instr ctx bb =
  match curr_instr with
  | None -> print_endline "at end of block"
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
          match ctx.state with
          | Looking ->
              let _ = print_endline "found load ptr" in
              let new_ctx = { tape_cell = None; state = FoundLoadPtr } in
              fold_loop_state_machine the_module next_instr new_ctx bb
          | FoundGep ->
              let _ = print_endline "found load tape" in
              let new_ctx = { tape_cell = curr_instr; state = FoundLoadTape } in
              fold_loop_state_machine the_module next_instr new_ctx bb
          | _ -> ())
      | GetElementPtr -> (
          match ctx.state with
          | FoundLoadPtr ->
              let _ = print_endline "found gep" in
              let new_ctx = { tape_cell = None; state = FoundGep } in
              fold_loop_state_machine the_module next_instr new_ctx bb
          | _ -> ())
      | Sub -> (
          match ctx.state with
          | FoundLoadTape ->
              let _ = print_endline "found sub" in
              let new_ctx = { tape_cell = ctx.tape_cell; state = FoundSub } in
              fold_loop_state_machine the_module next_instr new_ctx bb
          | _ -> ())
      | Store -> (
          match ctx.state with
          | FoundSub ->
              let _ = print_endline "found store" in
              let new_ctx = { tape_cell = ctx.tape_cell; state = FoundStore } in
              fold_loop_state_machine the_module next_instr new_ctx bb
          | _ -> ())
      | Br -> (
          match ctx.state with
          | FoundStore ->
              let _ = print_endline "found br, starting to fold" in
              fold the_module ctx.tape_cell bb
          | _ -> ())
      | _ -> print_endline "Not valid state")

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
          let _ = print_endline "new bb" in
          let ctx = { tape_cell = None; state = Looking } in
          let _ = fold_loop_state_machine the_module first_instr ctx bb in
          ())
        fn)
    the_module
