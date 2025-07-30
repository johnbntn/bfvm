type outfile = Object | Asm | IR | Exe
type opt_level = O0 | O1

val run_compiler : unit -> 'a
val compile : string -> string -> outfile -> opt_level -> unit
