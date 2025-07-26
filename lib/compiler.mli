type outfile = Object | Asm | IR | Exe

val run_compiler : unit -> 'a
val compile : string -> string -> outfile -> unit
