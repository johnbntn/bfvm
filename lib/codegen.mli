type env = {
  the_context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  builder: Llvm.llbuilder;
  ptr: Llvm.llvalue;
  tape: Llvm.llvalue
}

val generate : Parser.token list -> Llvm.llmodule