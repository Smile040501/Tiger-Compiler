(* User Declarations *)

(* The type of should match with type of `pos` in tiger.grm *)
type pos    = int

(* Stuff done to make use of Tokens module generated by tiger.grm *)
type svalue         = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult      = (svalue, pos) token

(* Keeps track of the line number *)
val lineNo    : pos ref = ref 0

(* Updates the input reference variable *)
fun incRef   x n = (x := !x + n)
fun decRef   x n = (x := !x - n)
fun resetRef x   = (x := 0)

(* Called by the lexer when the end of the input stream is reached. *)
fun eof () = ((resetRef lineNo); Tokens.EOF (!lineNo, !lineNo))

(* Some helper functions during lexing *)

(* ord : char -> int  (* Returns the ASCII value of the character *) *)
fun charsToInt m (x :: xs) = charsToInt (10 * m + ((ord x) - (ord #"0"))) xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =    charsToInt 0 xs
  | toSigned xs           =    charsToInt 0 xs

(* String.explode : string -> char list *)
val toInt = toSigned o String.explode

(* Counts number of newlines based on the \n's encountered in the input *)
val newLineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

%%

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

space    = [\ ];
tabspace = [\t];
ws       = [\ \t];
digit    = [0-9];
id       = [a-zA-Z][a-zA-Z0-9_]*;
nl       = (\n|\r\n|\r|\n\r);
alpha    = [a-zA-Z];

%%

<INITIAL> {nl}({ws}*{nl})*      => (incRef lineNo (newLineCount yytext); lex());
<INITIAL> {ws}+                 => (lex());

<INITIAL> "print"               => (Tokens.PRINT  (yypos, yypos + (size yytext)));
<INITIAL> "println"             => (Tokens.PRINTLN(yypos, yypos + (size yytext)));

<INITIAL> "for"                 => (Tokens.FOR (yypos, yypos + (size yytext)));
<INITIAL> "to"                  => (Tokens.TO  (yypos, yypos + (size yytext)));
<INITIAL> "by"                  => (Tokens.BY  (yypos, yypos + (size yytext)));
<INITIAL> "do"                  => (Tokens.DO  (yypos, yypos + (size yytext)));
<INITIAL> "done"                => (Tokens.DONE(yypos, yypos + (size yytext)));

<INITIAL> ":="                  => (Tokens.ASSIGN   (yypos, yypos + (size yytext)));
<INITIAL> ";"                   => (Tokens.SEMICOLON(yypos, yypos + (size yytext)));

<INITIAL> "+"                   => (Tokens.PLUS (yypos, yypos + (size yytext)));
<INITIAL> "-"                   => (Tokens.MINUS(yypos, yypos + (size yytext)));
<INITIAL> "*"                   => (Tokens.MUL  (yypos, yypos + (size yytext)));
<INITIAL> "/"                   => (Tokens.DIV  (yypos, yypos + (size yytext)));

<INITIAL> "("                   => (Tokens.LPAREN(yypos, yypos + (size yytext)));
<INITIAL> ")"                   => (Tokens.RPAREN(yypos, yypos + (size yytext)));


<INITIAL> {digit}+              => (Tokens.INT(toInt yytext, yypos, yypos + (size yytext)));
<INITIAL> {id}                  => (Tokens.ID (yytext, yypos, yypos + (size yytext)));
