[@@@warning "-37"]

type fold_ptr_state =
  | Looking
  | FirstLoad
  | FirstArith
  | FoldingStore
  | FoldingLoad
  | FoldingArith

type fold_ptr_ctx = {
  fold_instr : Llvm.llvalue option;
  accum : int64;
  state : fold_ptr_state;
}

[@@@warning "-27"]

let fold ctx end_instr = ()

let rec get_opcodes the_module curr_instr ctx =
  match curr_instr with
  | None -> print_endline "at end of block"
  | Some instr -> (
      let get_next instr =
        match Llvm.instr_succ instr with
        | At_end _ -> None
        | Before instr -> Some instr
      in
      let op = Llvm.instr_opcode instr in
      let open Llvm.Opcode in
      match op with
      | Load -> (
          let _ = print_endline "got a load" in
          let next_instr = get_next instr in
          match ctx.state with
          | Looking ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0L; state = FirstLoad }
          | FoldingStore ->
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum;
                  state = FoldingLoad;
                }
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0L; state = Looking })
      | Add -> (
          let _ = print_endline "got an add" in
          let next_instr = get_next instr in
          match ctx.state with
          | FirstLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = curr_instr;
                  accum = Int64.add ctx.accum 1L;
                  state = FirstArith;
                }
          | FoldingLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = Int64.add ctx.accum 1L;
                  state = FoldingArith;
                }
          | FoldingStore -> fold ctx curr_instr
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0L; state = Looking })
      | Sub -> (
          let _ = print_endline "got a sub" in
          let next_instr = get_next instr in
          match ctx.state with
          | FirstLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = curr_instr;
                  accum = Int64.sub ctx.accum 1L;
                  state = FirstArith;
                }
          | FoldingLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = Int64.sub ctx.accum 1L;
                  state = FoldingArith;
                }
          | FoldingStore -> fold ctx curr_instr
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0L; state = Looking })
      | Store -> (
          let _ = print_endline "got a store" in
          let next_instr = get_next instr in
          match ctx.state with
          | FirstArith ->
              let _ = print_endline "i should optimize here" in

              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum;
                  state = FoldingStore;
                }
          | FoldingArith ->
              let _ =
                print_endline ("we can fold " ^ Int64.to_string ctx.accum)
              in
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum;
                  state = FoldingStore;
                }
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0L; state = Looking })
      | _ -> (
          match ctx.state with
          | FoldingStore ->
              let _ =
                print_endline ("starting to fold " ^ Int64.to_string ctx.accum)
              in
              fold ctx curr_instr
          | _ ->
              let _ = print_endline "Not yet doing that" in
              let next_instr = get_next instr in
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0L; state = Looking }))

let fold_global_ops the_module =
  Llvm.iter_functions
    (fun fn ->
      Llvm.iter_blocks
        (fun blk ->
          let first_instr =
            match Llvm.instr_begin blk with
            | At_end _ -> None
            | Before instr -> Some instr
          in
          let ctx = { fold_instr = None; accum = 0L; state = Looking } in
          let _ = get_opcodes the_module first_instr ctx in
          ())
        fn)
    the_module
