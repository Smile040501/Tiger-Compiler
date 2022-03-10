(* Pretty Prints the AST of the Tiger Language *)
signature PRINT_TIGER_AST =
sig
    val print : Tiger.Prog -> unit
end

(* Structure to print the AST generated for the Tiger Language *)
structure PrintTigerAST :> PRINT_TIGER_AST =
struct
    open Tiger;

    (* Prints a string to stdout *)
    fun printString s = TextIO.output (TextIO.stdOut, s)

    (* Concatenates list of strings *)
    and concatStrings []      = ""
      | concatStrings (x::xs) = x ^ (concatStrings xs)

    (* Convert identifier to string *)
    and strId (identifier: id) = identifier

    (* Convert Integer to string *)
    and strInt n = Int.toString n

    (* Convert Expr datatype to list of strings *)
    and strExpr Nil            = ["Nil"]
      | strExpr (Int n)        = ["Int(", strInt n, ")"]
      | strExpr (Lval l)       = ["Lval("] @ (strLvalue l) @ [")"]
      | strExpr (Op operation) = let
                                    val {left, oper, right} = operation
                                    val strLeft  = strExpr left
                                    val strOper  = strBinOp oper
                                    val strRight = strExpr right
                                 in
                                    ["Op({left = "] @ strLeft @ [", oper = "] @ strOper @ [", right = "] @ strRight @ ["})"]
                                 end
      | strExpr (Neg e)        = ["Neg("] @ (strExpr e) @ [")"]
      | strExpr (Assign r)     = let
                                    val {lvalue, expr} = r
                                    val strLval = strLvalue lvalue
                                    val strE    = strExpr expr
                                 in
                                    ["Assign({lvalue = "] @ strLval @ [", expr = "] @ strE @ ["})"]
                                 end
      | strExpr (Print e)      = ["Print("] @ (strExpr e) @ [")"]
      | strExpr (Exprs es)     = ["Exprs(["] @ (strExprs es) @ ["])"]

    (* Converts List of Expr datatype to list of strings *)
    and strExprs []      = []
      | strExprs (x::xs) = (case xs of
                                [] => ["\n    "] @ (strExpr x) @ ["\n"]
                               | _ => ["\n    "] @ (strExpr x) @ [", "] @ (strExprs xs)
                            )

    (* Converts BinOp datatype to list of string *)
    and strBinOp Plus  = ["Plus"]
      | strBinOp Minus = ["Minus"]
      | strBinOp Mul   = ["Mul"]
      | strBinOp Div   = ["Div"]

    (* Converts Lvalue datatype to list of strings *)
    and strLvalue (Var x) = ["Var(", strId x, ")"]

    (* Prints the Tiger program *)
    and print (Expression e) = printString (concatStrings (strExpr e))
end
