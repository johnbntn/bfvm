open Parser

type env = {
  the_context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  builder: Llvm.llbuilder;
  tape: Llvm.llvalue;
  ptr: Llvm.llvalue;
}

let codegen_op ~env:env token =
  match token with
  | op -> print_endline (token_to_string op)

let generate tokens = 
  let the_context = Llvm.create_context () in
  let the_module = Llvm.create_module the_context "program" in
  let builder = Llvm.builder the_context in
  let i8_type = Llvm.i8_type the_context in
  let i8_array_type = Llvm.array_type i8_type 30000 in
  let tape = Llvm.define_global "tape" (Llvm.const_null i8_array_type) the_module in
  let ptr = Llvm.define_global "ptr" (Llvm.const_null i8_type) the_module in

  (* Global program state *)
  let env = {the_context; the_module; builder; tape; ptr} in

  List.iter (codegen_op ~env:env) tokens

