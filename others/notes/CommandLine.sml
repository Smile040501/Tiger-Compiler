(*===========================================================================*)

(* Provides access to the name and the arguments used to invoke the currently running program *)
(* https://smlfamily.github.io/Basis/command-line.html *)
structure CommandLine :> COMMAND_LINE

(* The name used to invoke the current program *)
val CommandLine.name : unit -> string

(* The argument list used to invoke the current program *)
val CommandLine.arguments : unit -> string list
