(* Common utilities *)
structure Utils =
struct
    (* Constants *)
    val PRINT_INT_SYSCALL    : int = 1
    val PRINT_STRING_SYSCALL : int = 4
    val EXIT_SYSCALL         : int = 10

    val A0_REG : string = "$a0"
    val V0_REG : string = "$v0"

    (* Identity Functions *)
    fun identity     x           = x
    fun identityInt (x : int)    = x
    fun identityStr (x : string) = x

    (* Concatenates list of strings *)
    (* concatStrs : string list -> string *)
    fun concatStrings []        = ""
      | concatStrings (x :: xs) = x ^ (concatStrings xs)

    (* Prints the input string to the outstream *)
    (* val TextIO.output  : TextIO.outstream * string -> unit *)
    fun printOut str = TextIO.output(TextIO.stdOut, str)
    fun printErr str = TextIO.output(TextIO.stdErr, str)

    (* Raises an Exception *)
    fun throwErr (ex : string -> exn) (msg : string) =
                (printErr msg; raise ex ("\nERROR: " ^ msg ^ "\n"))

    (* Returns true if an element satisfy the binary predicate with all the elements of the list *)
    (* val all = fn : ('a * 'b -> bool) -> 'a -> 'b list -> bool *)
    fun all f x [] = true
      | all f x (y :: ys) = (f (x, y)) andalso (all f x ys)

    (* Returns those elements from list l1 which satisfy binary predicate with all the elements of the list l2 *)
    (* val filterValues = fn : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list *)
    fun filterValues _ []        _  = []
      | filterValues f (x :: xs) l2 = (if (all f x l2) then
                                            x :: (filterValues f xs l2)
                                        else filterValues f xs l2
                                      )
end
