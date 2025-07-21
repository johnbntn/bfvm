open Parser

type loop = {
  header_bb: Llvm.llbasicblock;   (* initial comparison *)
  body_bb: Llvm.llbasicblock;     (* loop body *)
  after_bb: Llvm.llbasicblock     (* loop exit *)
}

type env = {
  the_context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  builder: Llvm.llbuilder;
  main_fn: Llvm.llvalue;
  ptr: Llvm.llvalue;
  tape: Llvm.llvalue;
  loop_stack: loop Stack.t
}

let codegen_op ~env:env token =
  let i8_type = Llvm.i8_type env.the_context in
  let i32_type = Llvm.i32_type env.the_context in
  let i8_array_type = Llvm.array_type i8_type 30000 in
  match token with
  | T_INC ->
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let const_one = Llvm.const_int i32_type 1 in
    let inc_ptr = Llvm.build_add load_ptr const_one "inc_ptr" env.builder in
    let _ = Llvm.build_store inc_ptr env.ptr env.builder in
    ()
  | T_DEC -> 
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let const_one = Llvm.const_int i32_type 1 in
    let dec_ptr = Llvm.build_sub load_ptr const_one "dec_ptr" env.builder in
    let _ = Llvm.build_store dec_ptr env.ptr env.builder in
    ()
  | T_PLUS ->
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let tape_cell_addr = Llvm.build_gep i8_array_type env.tape 
      [|Llvm.const_int i32_type 0; load_ptr|] "tape_cell_addr" env.builder in
    let tape_cell_value = Llvm.build_load i8_type tape_cell_addr "tape_cell_val" env.builder in
    let const_one = Llvm.const_int i8_type 1 in
    let add_to_cell = Llvm.build_add tape_cell_value const_one "add_to_cell" env.builder in
    let _ = Llvm.build_store add_to_cell tape_cell_addr env.builder in 
    ()
  | T_MINUS ->
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let tape_cell_addr = Llvm.build_gep i8_array_type env.tape 
      [|Llvm.const_int i32_type 0; load_ptr|] "tape_cell_addr" env.builder in
    let tape_cell_value = Llvm.build_load i8_type tape_cell_addr "tape_cell_val" env.builder in
    let const_one = Llvm.const_int i8_type 1 in
    let sub_from_cell = Llvm.build_sub tape_cell_value const_one "sub_from_cell" env.builder in
    let _ = Llvm.build_store sub_from_cell tape_cell_addr env.builder in 
    ()
  | T_LBRAC -> 
    let num_loops = Int.to_string (Stack.length env.loop_stack) in
    let new_loop = {
      header_bb = Llvm.append_block env.the_context ("loop_header_" ^ num_loops) env.main_fn;
      body_bb = Llvm.append_block env.the_context ("loop_body_" ^ num_loops) env.main_fn;
      after_bb = Llvm.append_block env.the_context ("loop_after_" ^ num_loops) env.main_fn; 
    } in
    let _ = Stack.push new_loop env.loop_stack in
    let _ = Llvm.build_br new_loop.header_bb env.builder in
    let _ = Llvm.position_at_end new_loop.header_bb env.builder in
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let tape_cell_addr = Llvm.build_gep i8_array_type env.tape 
      [|Llvm.const_int i32_type 0; load_ptr|] "tape_cell_addr" env.builder in
    let tape_cell_value = Llvm.build_load i8_type tape_cell_addr "tape_cell_val" env.builder in
    let const_zero = Llvm.const_int i8_type 0 in
    let cmp = Llvm.build_icmp Llvm.Icmp.Ne tape_cell_value const_zero "cmp" env.builder in
    let _ = Llvm.build_cond_br cmp new_loop.body_bb new_loop.after_bb env.builder in
    let _ = Llvm.position_at_end new_loop.body_bb env.builder in
    ()
  | T_RBRAC ->
    let _ = match Stack.pop env.loop_stack with
    | loop -> 
      let _ = Llvm.build_br loop.header_bb env.builder in
      Llvm.position_at_end loop.after_bb env.builder
    | exception Stack.Empty -> failwith "Unmatched parentheses"
    in 
    ()
  | T_DOT ->
    let get_fn_type = Llvm.function_type i32_type [||] in
    let get_fn = Llvm.declare_function "getchar"  get_fn_type env.the_module in
    let get_fn_call = Llvm.build_call get_fn_type get_fn [||] "get_char" env.builder in
    let trunc_get = Llvm.build_trunc get_fn_call i8_type "trunc_get" env.builder in
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let tape_cell_addr = Llvm.build_gep i8_array_type env.tape 
      [|Llvm.const_int i32_type 0; load_ptr|] "tape_cell_addr" env.builder in
    let _ = Llvm.build_store trunc_get tape_cell_addr env.builder in
    ()
  | T_COMMA -> 
    let load_ptr = Llvm.build_load i32_type env.ptr "load_ptr" env.builder in
    let tape_cell_addr = Llvm.build_gep i8_array_type env.tape 
    [|Llvm.const_int i32_type 0; load_ptr|] "tape_cell_addr" env.builder in
    let tape_cell_value = Llvm.build_load i8_type tape_cell_addr "tape_cell_val" env.builder in
    let put_fn_type = Llvm.function_type i32_type [|i8_type|] in
    let get_fn = Llvm.declare_function "putchar"  put_fn_type env.the_module in
    let _ = Llvm.build_call put_fn_type get_fn [|tape_cell_value|] "get_char" env.builder in
    ()
  | op -> failwith ("Op " ^ (token_to_string op) ^ " not supported yet")

let generate tokens = 
  let the_context = Llvm.create_context () in
  let the_module = Llvm.create_module the_context "program" in
  let builder = Llvm.builder the_context in
  let i8_type = Llvm.i8_type the_context in
  let i8_array_type = Llvm.array_type i8_type 30000 in
  let tape = Llvm.define_global "tape" (Llvm.const_null i8_array_type) the_module in

  let i32_type = Llvm.i32_type the_context in
  let ptr = Llvm.define_global "ptr" (Llvm.const_int i32_type 0) the_module in

  let void_type = Llvm.void_type the_context in
  let main_fn = Llvm.define_function "main" (Llvm.function_type void_type [||]) the_module in
  Llvm.position_at_end (Llvm.entry_block main_fn) builder;

  (* Global program state *)
  let env = {the_context; the_module; builder; main_fn; ptr; tape; loop_stack = Stack.create ()} in

  let _ = List.iter (codegen_op ~env:env) tokens in

  let _ = Llvm.build_ret_void builder in
  the_module
 