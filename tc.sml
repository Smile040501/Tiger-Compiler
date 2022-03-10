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

	(* Build Lexers *)
	(* val inputN : (instream * int) -> string *)
	fun makeTigerLexer strm = TigerParser.makeLexer(fn n => TextIO.inputN(strm, n))
	val makeFileLexer       = makeTigerLexer o TextIO.openIn


	(* Parse command line and set a suitable lexer *)
	val thisLexer = case CommandLine.arguments() of
			   []  => makeTigerLexer TextIO.stdIn
			|  [x] => makeFileLexer x
			|  _   => (TextIO.output(TextIO.stdErr, "usage: tc file");
					   OS.Process.exit OS.Process.failure)



	fun print_error (s, i:int, _) = TextIO.output(TextIO.stdErr,
							"Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

	val (program, _) = TigerParser.parse(0, thisLexer, print_error, ()) (* parsing *)
	val _ = PrintTigerAST.print program
	val (irProg: Ir.Prog, env: Env.mp)  = Translate.compileToIR (Env.empty ()) program

end
