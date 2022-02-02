(* This type captures the range (or return type) of the lexer. You
   should think of lexer as a function that takes a string and returns
   the next token which in this case happens to be Machine.Inst option
 *)

type lexresult             = Machine.Inst option

(* The token to return when the end of stream is reached.*)
fun eof ()                 = NONE

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode

%%

%structure RPLex
whitespace=[\ \r\t\n];
digit=[0-9];

%%
{whitespace}+     => (lex()  (* White spaces are ignored *) );
"#".*\n           => (lex()  (* A line comment *)           );
[+-~]?{digit}+    => (SOME (Machine.Push (toInt yytext)));
"+"               => (SOME (Machine.Exec Ast.Plus      ));
"-"               => (SOME (Machine.Exec Ast.Minus     ));
"*"               => (SOME (Machine.Exec Ast.Mul       ));
"/"               => (SOME (Machine.Exec Ast.Div       ));
"p"               => (SOME (Machine.PrintTop));
"s"               => (SOME (Machine.PrintStack));
"c"               => (SOME (Machine.ClearStack));
