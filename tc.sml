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
					\-D\n\
					\--debug\n\
					\   Display all the required information for debugging.\
					\-F\n\
					\--file\n\
					\    Execute the source tiger file. This is the \027[31mdefault\027[0m.\
					\-I\n\
					\--ir\n\
					\    Display the IR generated from the source file.\n\n\
					\-R\n\
					\--reg-alloc\n\
					\    Display the register allocation performed by the compiler.\n\n\
					\-T\n\
					\--temp-alloc\n\
					\    Display the temporaries allocation performed by the compiler.\n\n\
					\-S\n\
					\--asm\n\
					\    Display the final assembler code.\n\n"

	(* Outputs the `man` string and terminates the program *)
	(* val OS.Process.failure : OS.Process.status *)
	(* val OS.Process.exit : OS.Process.status -> 'a *)
	fun failExit () = (Utils.printErr manString; OS.Process.exit OS.Process.failure)

	(* Successfully exit the program *)
	fun successExit () = OS.Process.exit OS.Process.success

	(* Returns the flag and the filename from the input *)
	(* val getFlagAndFile : stirng -> string -> string * string *)
	fun getFlagAndFile x y =
		let
			val flagsList = ["-?", "--help", "-A", "--ast", "-D", "--debug", "-F", "--file", "-I", "--ir", "-R", "--reg-alloc", "-T", "--temp-alloc", "-S", "--asm"]
			val flagX = List.exists (fn a => a = x) flagsList
			val flagY = List.exists (fn a => a = y) flagsList
		in
			(case (flagX, flagY) of
				  (true, true)   => failExit()
				| (true, false)  => (x, y)
				| (false, true)  => (y, x)
				| (false, false) => failExit()
			)
		end

	val flagRef : string ref = ref ""	(* Remembers the flag being passed as input *)

	(* Parse command line and set a suitable lexer *)
	(* val CommandLine.arguments : unit -> string list *)
	val tokenStream = case CommandLine.arguments() of
			  []     => makeTigerLexer TextIO.stdIn
			| [x]    =>
				(if x = "-?" orelse x = "--help" then
					(flagRef := x; makeTigerLexer (TextIO.openString ""))
				 else makeFileLexer x
				)
			| [x, y] =>
				let
					val (flag, file) = getFlagAndFile x y
					val  _           = flagRef := flag
				in
					makeFileLexer file
				end
			|  _     => failExit()


	(* Displays the `man` string for compiler usage *)
	fun displayHelpMsg () = Utils.printOut manString

	val _ = if (!flagRef = "-?" orelse !flagRef = "--help") then
				(displayHelpMsg(); successExit(); ())
			else ()


	(* print_error function as exepcted by the parser *)
	fun print_error (s, i : int, _) = Utils.printErr
										("Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

	(* Parsing the token stream generated by the lexer *)
	val (tigerProgram : Tiger.Prog, _) = TigerParser.parse(0, tokenStream, print_error, ())


	(* Displays the Tiger AST generated from the source file *)
	fun displayTigerAST () = Utils.printOut (PrettyTigerAST.prettyTig tigerProgram)

	val _ = if (!flagRef = "-A" orelse !flagRef = "--ast") then
				(displayTigerAST(); successExit(); ())
			else ()


	(* Generating the intermediate representation of the Tiger AST *)
	val (irProgram: Ir.Prog, env: Env.mp)  = Translate.compileToIR (Env.empty ()) tigerProgram

	(* Displays the intermediate representation of the Tiger AST *)
	fun displayIR () = Utils.printOut (Ir.prettyProg irProgram)

	val _ = if (!flagRef = "-I" orelse !flagRef = "--ir") then
				(displayIR(); successExit(); ())
			else ()

	(* Display the temporaries allocation performed by the compiler *)
	fun displayTempAlloc () =
				let
					val temps = Env.listItems env
					(* The temporaries allocation performed by the compiler *)
					(* val temps : (string * Temp.value) list *)
				in
					Utils.printPairList Utils.identity Temp.prettyValue temps
				end

	val _ = if (!flagRef = "-T" orelse !flagRef = "--temp-alloc") then
				(displayTempAlloc(); successExit(); ())
			else ()

	(* Displays the register allocation performed by the compiler *)
	fun displayRegAlloc () =
					let
						val regs = RegAlloc.listItems ()
						(* The register allocation performed by the compiler *)
						(* val regs : (string * string) list *)
					in
						Utils.printPairList Utils.identity Utils.identity regs
					end

	val _ = if (!flagRef = "-R" orelse !flagRef = "--reg-alloc") then
				(displayRegAlloc(); successExit(); ())
			else ()

	val _ = if (!flagRef = "-D" orelse !flagRef = "--debug") then
				(Utils.printOut "---------------TIGER AST---------------\n"; displayTigerAST();
				Utils.printOut "---------------IR---------------\n"; displayIR();
				Utils.printOut "---------------TEMP ALLOC---------------\n"; displayTempAlloc();
				Utils.printOut "---------------REG ALLOC---------------\n"; displayRegAlloc();
				Utils.printOut "---------------OUTPUT---------------\n";
				())
			else ()

	(* Generating the final MIPS program *)
	val mipsProgram = Translate.compileToMips irProgram

	(* The stringified representation of the MIPS program that SPIM will understand *)
	val output = PrettyMips.prettyMapProg Utils.identity PrettyMips.prettyReg mipsProgram

	(* Displays the final output *)
	val _ = Utils.printOut output
end
