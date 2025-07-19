open Parser

(* Still testing, not using everything yet, will remove when done *)
[@@@warning "-69"]
type env = {
  the_context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  builder: Llvm.llbuilder;
  ptr: Llvm.llvalue;
  tape: Llvm.llvalue
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
    let sub_to_cell = Llvm.build_sub tape_cell_value const_one "sub_to_cell" env.builder in
    let _ = Llvm.build_store sub_to_cell tape_cell_addr env.builder in 
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

  (* Global program state *)
  let env = {the_context; the_module; builder; ptr; tape} in

  (* create main function and position builder *)
  let main_function = Llvm.define_function "main" (Llvm.function_type i32_type [||]) the_module in
  Llvm.position_at_end (Llvm.entry_block main_function) builder;

  let _ = List.iter (codegen_op ~env:env) tokens in

  let _ = Llvm.build_ret (Llvm.const_int i32_type 0) builder in
  the_module
 