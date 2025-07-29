[@@@warning "-37"]

type fold_global_state =
  | Looking
  | FirstLoad
  | FirstArith
  | FoldingStore
  | FoldingLoad
  | FoldingArith

type fold_global_ctx = {
  fold_instr : Llvm.llvalue option;
  accum : int;
  state : fold_global_state;
}

(* Only use this when you know the instruction exists *)
let get_instr_succ instr =
  Option.get
    (match instr with Llvm.At_end _ -> None | Llvm.Before instr -> Some instr)

let unwrap_instr_pred instr =
  match instr with Llvm.At_start _ -> None | Llvm.After instr -> Some instr

[@@@warning "-32"]

let rec iterate_and_delete_until curr_instr end_instr =
  if curr_instr == end_instr then
    let _ =
      print_endline
        ("deleting " ^ Llvm.string_of_llvalue curr_instr ^ " it's the last one!")
    in
    let _ = Llvm.delete_instruction curr_instr in
    ()
  else
    let next_instr = get_instr_succ (Llvm.instr_succ curr_instr) in
    let _ = print_endline ("deleting " ^ Llvm.string_of_llvalue curr_instr) in
    let _ = Llvm.delete_instruction curr_instr in
    iterate_and_delete_until next_instr end_instr

let fold ctx the_module end_instr_opt =
  let start_instr_opt =
    match ctx.fold_instr with
    | None -> failwith "Start of fold was none, this should never happen."
    | instr -> instr
  in
  let end_instr =
    match end_instr_opt with
    | None -> failwith "End instr none, this should never happen"
    | Some instr -> instr
  in
  let start_instr =
    match start_instr_opt with
    | None -> failwith "start instr none, this should never happen"
    | Some instr -> instr
  in

  let the_context = Llvm.module_context the_module in
  let builder = Llvm.builder the_context in

  let _ =
    print_endline
      ("start val is "
      ^ Llvm.string_of_llvalue start_instr
      ^ " end val is "
      ^ Llvm.string_of_llvalue end_instr)
  in

  let global = Option.get (unwrap_instr_pred (Llvm.instr_pred start_instr)) in
  let _ = print_endline "got global, not accessing though" in
  let _ = print_endline ("got global, it's " ^ Llvm.string_of_llvalue global) in
  let const_type = Llvm.type_of (Llvm.operand start_instr 1) in
  let _ = print_endline "got type of op" in
  let _ = iterate_and_delete_until start_instr end_instr in
  let _ = Llvm.position_builder (Llvm.instr_succ global) builder in
  let _ = print_endline "moved builder" in
  let _ = print_endline "done deleting" in
  let _ = print_endline ("got global, it's " ^ Llvm.string_of_llvalue global) in
  if ctx.accum = 0 then
    let _ = Llvm.delete_instruction global in
    print_endline "do nothing"
  else if ctx.accum < 0 then
    let _ = print_endline "neg" in
    let const_accum = Llvm.const_int const_type (Int.abs ctx.accum) in
    let _ =
      print_endline ("got accum val, it's " ^ Llvm.string_of_llvalue const_accum)
    in
    let fold_sub = Llvm.build_sub global const_accum "sub_fold" builder in
    let _ = print_endline ("new instr is " ^ Llvm.string_of_llvalue fold_sub) in
    let _ = Llvm.build_store fold_sub global builder in
    ()
  else if ctx.accum > 0 then
    let _ = print_endline "pos" in
    let const_accum = Llvm.const_int const_type ctx.accum in
    let _ =
      print_endline ("got accum val, it's " ^ Llvm.string_of_llvalue const_accum)
    in
    let fold_add = Llvm.build_add global const_accum "add_fold" builder in
    let _ = print_endline ("new instr is " ^ Llvm.string_of_llvalue fold_add) in
    let _ = Llvm.build_store fold_add global builder in
    ()

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
                { fold_instr = None; accum = 0; state = FirstLoad }
          | FoldingStore ->
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum;
                  state = FoldingLoad;
                }
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0; state = Looking })
      | Add -> (
          let _ = print_endline "got an add" in
          let next_instr = get_next instr in
          match ctx.state with
          | FirstLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = curr_instr;
                  accum = ctx.accum + 1;
                  state = FirstArith;
                }
          | FoldingLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum + 1;
                  state = FoldingArith;
                }
          | FoldingStore ->
              fold ctx the_module
                (unwrap_instr_pred (Llvm.instr_pred (Option.get curr_instr)))
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0; state = Looking })
      | Sub -> (
          let _ = print_endline "got a sub" in
          let next_instr = get_next instr in
          match ctx.state with
          | FirstLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = curr_instr;
                  accum = ctx.accum - 1;
                  state = FirstArith;
                }
          | FoldingLoad ->
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum - 1;
                  state = FoldingArith;
                }
          | FoldingStore ->
              fold ctx the_module
                (unwrap_instr_pred (Llvm.instr_pred (Option.get curr_instr)))
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0; state = Looking })
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
                print_endline ("we can fold " ^ Int.to_string ctx.accum)
              in
              get_opcodes the_module next_instr
                {
                  fold_instr = ctx.fold_instr;
                  accum = ctx.accum;
                  state = FoldingStore;
                }
          | _ ->
              get_opcodes the_module next_instr
                { fold_instr = None; accum = 0; state = Looking })
      | Ret -> (
          let _ = print_endline "got a ret" in
          match ctx.state with
          | FoldingStore ->
              fold ctx the_module
                (unwrap_instr_pred (Llvm.instr_pred (Option.get curr_instr)))
          | _ -> ())
      | _ ->
          let _ = print_endline "Not yet doing that" in
          let next_instr = get_next instr in
          get_opcodes the_module next_instr
            { fold_instr = None; accum = 0; state = Looking })

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
          let ctx = { fold_instr = None; accum = 0; state = Looking } in
          let _ = get_opcodes the_module first_instr ctx in
          ())
        fn)
    the_module
