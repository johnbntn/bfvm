type file_type = Object | Asm | IR | Exe  (** File emitted to *)

type opt_level =
  | O0
  | O1  (** O0 for no optimizations, O1 for [Passes.fold_loops] *)

val run_compiler : unit -> 'a
(** [run_compiler ()] runs the compiler with Cmdliner to parse args *)

val compile : string -> string -> file_type -> opt_level -> unit
(** [compile infile outfile file_type opt_level] compiles [infile] to [outfile]
    of type [file_type] at optimization level [opt_level] *)
