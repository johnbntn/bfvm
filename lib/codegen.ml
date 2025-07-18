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
  let _i8_type = Llvm.i8_type env.the_context in
  let i32_type = Llvm.i32_type env.the_context in
  let _i8_array_type = Llvm.array_type _i8_type 30000 in
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
  | op -> failwith ("Op " ^ (token_to_string op) ^ " not supported")

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
 