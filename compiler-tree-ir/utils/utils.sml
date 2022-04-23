(* Common utilities *)
structure Utils =
struct
    (* Constants *)
    val PRINT_INT_SYSCALL    : int = 1
    val PRINT_STRING_SYSCALL : int = 4
    val EXIT_SYSCALL         : int = 10

    val A0_REG : string = "$a0"   (* For register a0 *)
    val V0_REG : string = "$v0"   (* For register v0 *)

    val NL_Label : string = "nl"  (* For newline label *)

    (* Exceptions *)
    exception EmptyList of string

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

    (* Outputs the `man` string and terminates the program *)
    (* val OS.Process.failure : OS.Process.status *)
    (* val OS.Process.exit : OS.Process.status -> 'a *)
    fun failExit msg = (printErr msg; OS.Process.exit OS.Process.failure)

    (* Successfully exit from the program *)
	  fun successExit () = OS.Process.exit OS.Process.success

    (* Returns true if an element satisfy the binary predicate with all the elements of the list *)
    (* val all : ('a * 'b -> bool) -> 'a -> 'b list -> bool *)
    fun all f x [] = true
      | all f x (y :: ys) = (f (x, y)) andalso (all f x ys)

    (* Returns those elements from list l1 which satisfy binary predicate with all the elements of the list l2 *)
    (* val filterValues : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list *)
    fun filterValues _ []        _  = []
      | filterValues f (x :: xs) l2 = (if (all f x l2) then
                                            x :: (filterValues f xs l2)
                                        else filterValues f xs l2
                                      )

    (* Indents the input string by the given amount of spaces *)
    fun indent str 0 = str
      | indent str n = " " ^ (indent str (n-1))

    (* Pads the input string with the input character string upto length n on left *)
    (* val padLeft : int -> string -> string -> string *)
    fun padLeft n str c = case ((size str) >= n) of
                              true  => str
                            | false => padLeft n (c ^ str) c

    (* Pads the input string with the input character string upto length n on right *)
    (* val padRight : int -> string -> string -> string *)
    fun padRight n str c = case ((size str) >= n) of
                              true  => str
                            | false => padRight n (str ^ c) c


    (* Prints a pair of list *)
    (* val printPairList : ('a -> string) -> ('b -> string) -> ('a * 'b) list -> unit *)
    fun printPairList f g []             = ()
      | printPairList f g ((k, v) :: xs) =
              let
                  val lhs = padLeft 5 (f k) " "
                  val rhs = g v
              in
                  (printOut (lhs ^ ": " ^ rhs ^ "\n")); (printPairList f g xs)
              end

    (* Updates the first value of the list *)
    fun updateFirstVal []        _ = []
      | updateFirstVal (x :: xs) y = (y :: xs)

    (* Updates the last value of the list *)
    fun updateLastVal []        _ = []
      | updateLastVal [x]       y = [y]
      | updateLastVal (x :: xs) y = (x :: (updateLastVal xs y))

    (* Returns the last value of the list *)
    fun getLastVal []        = raise EmptyList "[utils.sml]:[getLastVal]: List is Empty\n"
      | getLastVal [x]       = x
      | getLastVal (x :: xs) = getLastVal xs
end
