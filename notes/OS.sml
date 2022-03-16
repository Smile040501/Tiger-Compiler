(*===========================================================================*)

(* Contained for a collection of structures for interacting with the operating system's file system,
    directory paths, processes, and I/O subsystem.
*)
(* https://smlfamily.github.io/Basis/os.html *)
structure OS :> OS

(* Provides facilities for accessing and operating on the file system      *)
(* https://smlfamily.github.io/Basis/os-file-sys.html#OS_FILE_SYS:SIG:SPEC *)
structure FileSys : OS_FILE_SYS

(* Provides support for manipulating the syntax of file system paths independent of the underlying file system *)
(* https://smlfamily.github.io/Basis/os-path.html#OS_PATH:SIG:SPEC *)
structure Path : OS_PATH

(* Splits the input path into its base and extension parts *)
val OS.Path.splitBaseExt : string -> {base : string, ext : string option}
(*
    Examples:
        ""	            {base = "",           ext = NONE}
        ".login"	    {base = ".login",     ext = NONE}
        "/.login"	    {base = "/.login",    ext = NONE}
        "a"      	    {base = "a",          ext = NONE}
        "a."	        {base = "a.",         ext = NONE}
        "a.b"	        {base = "a",          ext = SOME "b"}
        "a.b.c"	        {base = "a.b",        ext = SOME "c"}
        ".news/comp"	{base = ".news/comp", ext = NONE}
*)

(* More efficient *)
val OS.Path.base : string -> string
val OS.Path.ext  : string -> string option


(* Provides functions for manipulating processes in an operating system independent manner *)
(* https://smlfamily.github.io/Basis/os-process.html *)
structure Process : OS_PROCESS

(* Represents various termination conditions for processes, typically be an integral value *)
type OS.Process.status

(* Signifies successful termination of a process *)
val OS.Process.success : OS.Process.status

(* Signifies an error during execution of a process *)
val OS.Process.failure : OS.Process.status

(* Returns true if the status denotes success *)
val OS.Process.isSuccess : OS.Process.status -> bool

(* Passes the command string cmd to the operating system's default shell to execute *)
val OS.Process.system : string -> OS.Process.status

(* Registers an action f to be executed when the current SML program calls exit.
   Actions will be executed in the reverse order of registration.
*)
val OS.Process.atExit : (unit -> unit) -> unit

(* Executes all actions registered with atExit, flushes and closes all I/O streams opened using the Library, then terminates the SML process with termination status st *)
val OS.Process.exit : OS.Process.status -> 'a

(* Terminates the SML process with termination status st, without executing the actions registered with atExit or flushing open I/O streams *)
val OS.Process.terminate : OS.Process.status -> 'a
