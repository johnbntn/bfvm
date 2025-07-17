# BF Virtual Machine

Compiler for the [BF](https://en.wikipedia.org/wiki/Brainfuck) programming language, written in OCaml with the LLVM bindings.

### Installation Issues
When install llvm with
```bash
opam install llvm --verbose
```
I noticed that I was using `gcc` for the C compiler and `clang++` for the C++ compiler. This was causing an issue with the std::atomic check. Simply setting the `CC` and `CXX` to the same version fixed it. 