val generate : Parser.token list -> Llvm.llmodule
(** [generate tokens the_module] generates LLVM IR for your stream of tokens and
    places it into the_module *)
