(lang dune 3.17)

(name bfvm)

(generate_opam_files true)

(source
 (github johnbntn/bfvm))

(authors "John Benton <jbenton4494@gmail.com>")

(maintainers "John Benton <jbenton4494@gmail.com>")

(license MIT)

(documentation https://johnbntn.github.io/bfvm/bfvm/index.html)

(package
 (name bfvm)
 (synopsis "Brainf*ck Virtual Machine")
 (description "Compiler for the Brainf*ck programming language using LLVM")
 (depends ocaml alcotest)
 (tags
  ("llvm" "bf")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
