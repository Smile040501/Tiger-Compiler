(* Tiger Compiler *)
structure TC =
struct

	(* See `Creating the Parser` notes in ML-Yacc *)
	structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
	structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
	structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
								structure Lex = TigerLex
								structure LrParser=LrParser
							)


	(* Building the Lexers *)
	(* val inputN : (instream * int) -> string *)
	fun makeTigerLexer strm = TigerParser.makeLexer(fn n => TextIO.inputN(strm, n))
	(* val TextIO.openIn : string -> TextIO.instream *)
	val makeFileLexer       = makeTigerLexer o TextIO.openIn


	(* `man` page like string for displaying the usage of the compiler *)
	val manString = "usage: ./tc [OPTIONS] file.tig\n\n\
					\OPTIONS\n\
					\-------\n\
					\-?\n\
					\--help\n\
					\    Display the help message.\n\n\
					\-A\n\
					\--ast\n\
					\    Display the AST generated from the source file.\n\n\
					\-C\n\
					\--canon\n\
					\    Display the canonicalized IR generated from the source file.\n\n\
					\-D\n\
					\--debug\n\
					\   Display all the required information for debugging.\n\n\
					\-F\n\
					\--file\n\
					\    Execute the source tiger file. This is the \027[31mdefault\027[0m.\n\n\
					\-I\n\
					\--ir\n\
					\    Display the IR generated from the source file.\n\n\
					\-S\n\
					\--asm\n\
					\    Display the final assembler code.\n\n"


	(* Returns the flag and the filename from the input *)
	(* val getFlagAndFile : stirng -> string -> string * string *)
	fun getFlagAndFile x y =
		let
			val flagsList = ["-?", "--help", "-A", "--ast", "-C", "--canon", "-D", "--debug", "-F", "--file", "-I", "--ir", "-S", "--asm"]
			val flagX = List.exists (fn a => a = x) flagsList
			val flagY = List.exists (fn a => a = y) flagsList
		in
			(case (flagX, flagY) of
				  (true, true)   => Utils.failExit manString
				| (true, false)  => (x, y)
				| (false, true)  => (y, x)
				| (false, false) => Utils.failExit manString
			)
		end

	val flagRef  : string ref = ref ""	(* Remembers the flag being passed as input *)
	val fileName : string ref = ref ""  (* Remembers the file name being passed as input *)

	(* Terminates the process due to wrong input file extension *)
	fun wrongFileExtension () = (Utils.printErr "The file extension must be \027[31m.tig\027[0m.\n\n";
										Utils.failExit manString)

	(* Validates the file extension of the input file and stores the file name if correct *)
	fun checkFileExtension file =   let
										val base = OS.Path.base file
										val ext  = OS.Path.ext  file
										val _ = case ext of
											  SOME e => (if e = "tig" then fileName := base
											  			 else wrongFileExtension()
											  			)
											| NONE   => wrongFileExtension()
									in
										()
									end

	(* Parse command line and set a suitable lexer *)
	(* val CommandLine.arguments : unit -> string list *)
	val tokenStream = case CommandLine.arguments() of
			  []     => makeTigerLexer TextIO.stdIn
			| [x]    =>
				(if x = "-?" orelse x = "--help" then
					(flagRef := x; makeTigerLexer (TextIO.openString ""))
				 else (checkFileExtension x; makeFileLexer x)
				)
			| [x, y] =>
				let
					val (flag, file) = getFlagAndFile x y
					val  _           = flagRef := flag
					val  _           = checkFileExtension file
				in
					makeFileLexer file
				end
			|  _     => Utils.failExit manString


	(* Displays the `man` string for compiler usage *)
	fun displayHelpMsg () = Utils.printOut manString

	val _ = if (!flagRef = "-?" orelse !flagRef = "--help") then
				(displayHelpMsg(); Utils.successExit(); ())
			else ()


	(* print_error function as exepcted by the parser *)
	fun print_error (s, i : int, _) = Utils.printErr
										("Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

	(* Parsing the token stream generated by the lexer *)
	val (tigerProgram : Tiger.Prog, _) = TigerParser.parse(0, tokenStream, print_error, ())


	(* Displays the Tiger AST generated from the source file *)
	fun displayTigerAST () = Utils.printOut (PrettyTigerAST.prettyTig tigerProgram)

	val _ = if (!flagRef = "-A" orelse !flagRef = "--ast") then
				(displayTigerAST(); Utils.successExit(); ())
			else ()


	(* Generating the intermediate representation of the Tiger AST *)
	val irCodeStmt = Translate.compileToIR tigerProgram

	(* Displays the intermediate representation of the Tiger AST *)
	fun displayIR () = Utils.printOut (PrettyTree.prettyTreeStm irCodeStmt)

	val _ = if (!flagRef = "-I" orelse !flagRef = "--ir") then
				(displayIR(); Utils.successExit(); ())
			else ()


	(* Generating the canonicalized intermediate representation of the Tiger AST *)
	val canonicalizedIR = Canon.linearize irCodeStmt

	(* Displays the canonicalized intermediate representation of the Tiger AST *)
	fun displayCanonicalizedIR () = map (Utils.printOut o PrettyTree.prettyTreeStm) canonicalizedIR

	val _ = if (!flagRef = "-C" orelse !flagRef = "--canon") then
				(displayCanonicalizedIR(); Utils.successExit(); ())
			else ()


	(* Generating the final MIPS program *)
	val mipsProgram = CodeGen.generateMipsProg canonicalizedIR

	(* The stringified representation of the MIPS program that SPIM will understand *)
	val assemblerCode = PrettyMips.prettyMapProg Utils.identity PrettyMips.prettyReg mipsProgram

	(* Displays the assembler code *)
	fun displayAssembler () = Utils.printOut assemblerCode

	val _ = if (!flagRef = "-S" orelse !flagRef = "--asm") then
				(displayAssembler(); Utils.successExit(); ())
			else ()

	(* Debugging mode *)
	val _ = if (!flagRef = "-D" orelse !flagRef = "--debug") then
				(Utils.printOut "---------------TIGER AST---------------\n";
				displayTigerAST();
				Utils.printOut "---------------IR---------------\n";
				displayIR();
				Utils.printOut "---------------CANONICALIZED IR---------------\n"; displayCanonicalizedIR();
				Utils.printOut "---------------ASSEMBLER CODE---------------\n";
				displayAssembler();
				Utils.successExit();
				())
			else ()

	(* Write the assembly code to `fileName.s` *)
	val _ = TextIO.output(TextIO.openOut ((!fileName) ^ ".s"), assemblerCode)
end
