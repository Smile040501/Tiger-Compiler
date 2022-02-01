(** * Reverse polish calculator

This is a reverse polish calculator. This is meant to be compiled into
a standalone executable using mlton. The reverse polish calculator
described here usually takes multi-line integer expressions in reverse
polish notation and executes the corresponding code.  Besides integers
and the operators +, *, and -, the calculator understands the
following command.

p - print the top of the stack
s - print the entire stack
c - clear the stack.
# - starts a line comment. Ends when the line ends.

*)

structure RP =
struct

(** ** Using the lexer

We need the lexer function that keeps returning the tokens. This lexer
function is created by the `makeLexer` function exported by the
structure RPLex structure that `ml-lex` creates for us from `rp.lex`
file.

The `makeLexer` function in turn needs a way to read the input stream
of characters that it converts to tokens. This is done by passing to
`makeLexer` a function that reads the necessary character,
i.e. `makeLexer` takes as input a function `foo : int -> string`. When
the underlying lexer needs some input character to continue, it calls
this argument function to read it.

In the code below we make two different variants for the lexer, one
that is for interactive run of rp and reads one character at a time.
The other is of lexing entire files.

*)

(*

This is the interactive version, and hence the makeLexer function is
passed, the function that reads the next 1 character. We ignore the
demand of `n` character by the lexer because we want better
interactive responsiveness at the cost of some efficiency.


*)

val interactive = RPLex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))

(* On the other hand when reading from a file. We want to read as much as possible
   from the fine
*)
fun lexfile file = let val strm = TextIO.openIn file
		   in RPLex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end

(*

TODO: For standard input which is not from the terminal, we would like
to use a more efficient lexer than interactive. This will be relevent
when we want to pipe the output to rp.

*)


(* Running with a lexer *)
fun runWithLexer lexer = let fun loop stack = case lexer () of
						  NONE      => ()
					       |  SOME inst => loop (Machine.step inst stack)
			 in loop []
			 end


val _ =  ( case CommandLine.arguments() of
	       [] => runWithLexer interactive
	    |  xs => (List.map (runWithLexer o lexfile) xs; ())
	 )
	 handle Machine.StackUnderflow s =>
		( print "error: Stack underflow: " ;
		  Machine.printstack s;
		  OS.Process.exit OS.Process.failure
		)

end
