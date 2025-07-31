# BF Virtual Machine

Compiler for the [BF](https://en.wikipedia.org/wiki/Brainfuck) programming language, written in OCaml with the [LLVM](https://ocaml.org/p/llvm/latest/doc/index.html) bindings.

### Installation

To install, clone this repository and then run
```bash
opam install .
```

### Documentation

The documentation for this package is hosted [here](https://johnbntn.github.io/bfvm/bfvm/index.html)

### Installation Issues
When installing llvm with
```bash
opam install llvm --verbose
```
I noticed that I was using `gcc` for the C compiler and `clang++` for the C++ compiler. This was causing an issue with the `std::atomic` check. Simply setting the `CC` and `CXX` environment variables both to clang fixed it, would likely both work with gcc as well.
