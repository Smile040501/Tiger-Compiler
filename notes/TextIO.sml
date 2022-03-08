(*===========================================================================*)
(* Input and Output *)

(*
These are the abstract types of stream elements and vectors of elements.
For text streams, these are `Char.char` and `String.string`,
while for binary streams, they correspond to `Word8.word` and `Word8Vector.vector`
*)
type TextIO.vector = StreamIO.vector
type TextIO.elem = StreamIO.elem

(* Type of redirectable input and output stream *)
type TextIO.instream
type TextIO.outstream

(* Correspond to standard input, output and error streams *)
val TextIO.stdIn  : TextIo.instream
val TextIO.stdOut : TextIO.outstream
val TextIO.stdErr : TextIO.outstream

(* Attempts to read from the input stream *)
val TextIO.input    : TextIO.instream -> TextIO.vector
(* Reads 1 element from stream. Returns SOME(e) if 1 element is available, NONE if at end-of-stream *)
val TextIO.input1   : TextIO.instream -> TextIO.elem option
(* Reads at most n elements from stream *)
val TextIO.inputN   : TextIO.instream * int -> vector
(* Returns all elements of stream up to end-of-stream *)
val TextIO.inputAll : TextIO.instream -> TextIO.vector
(* Attempts to read all characters from current position up to and including the next newline *)
val TextIO.inputLine : TextIO.instream -> string option

(* Closes the input stream *)
val TextIO.closeIn : TextIO.instream -> unit

(* Returns `true` if the stream is at the end-of-stream *)
val TextIO.endOfStream : TextIO.instream -> bool

(* Attempts to write the contents to the output stream *)
val TextIO.output  : TextIO.outstream * TextIO.vector -> unit
val TextIO.output1 : TextIO.outstream * TextIO.elem -> unit

(* Cause any buffer associated with the stream to be written out *)
val TextIO.flushout : TextIO.outstream -> unit

(* Closes the output stream *)
val TextIO.closeOut : TextIO.outstream -> unit


(* Open the file named `name` for input *)
val TextIO.openIn : string -> TextIO.instream

(*
-   Open the file named `name` for output
-   File will be created if it doesn't exist
-   Truncates the file to length zero
*)
val TextIO.openOut    : string -> TextIO.outstream
val TextIO.openAppend : string -> TextIO.outstream  (* Opens the file in append mode *)

(* Creates an input stream whose content is the input string `s` *)
val TextIO.openString : string -> TextIO.instream

(* Prints the string `s` to stdout and flushes the stream *)
(* Equivalent to: `(TextIO.output (TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut)` *)
val TextIO.print : string -> unit
