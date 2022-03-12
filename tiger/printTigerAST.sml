(* Pretty Prints the AST of the Tiger Language *)
signature PRINT_TIGER_AST =
sig
    val prettyTig : Tiger.Prog -> string
    val print     : Tiger.Prog -> unit
end

(* Structure to print the AST generated for the Tiger Language *)
structure PrintTigerAST :> PRINT_TIGER_AST =
struct
    open Tiger;

    (* Convert Integer to string *)
    (* val strInt : int -> string *)
    fun strInt n = Int.toString n

    (* Convert Expr datatype to list of strings *)
    (* strExpr : Tiger.Expr -> string list *)
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
    (* strExprs : Tiger.Expr list -> string list *)
    and strExprs []      = []
      | strExprs (x::xs) = (case xs of
                                [] => ["\n    "] @ (strExpr x) @ ["\n"]
                               | _ => ["\n    "] @ (strExpr x) @ [", "] @ (strExprs xs)
                            )

    (* Converts BinOp datatype to list of string *)
    (* strExpr : Tiger.BinOp -> string list *)
    and strBinOp Plus  = ["Plus"]
      | strBinOp Minus = ["Minus"]
      | strBinOp Mul   = ["Mul"]
      | strBinOp Div   = ["Div"]

    (* Converts Lvalue datatype to list of strings *)
    (* strLvalue : Tiger.Lvalue -> string list *)
    and strLvalue (Var x) = ["Var(", x, ")"]

    (* Returns the string representation of the Tiger AST *)
    (* prettyTig : Tiger.Prog -> string *)
    and prettyTig (Expression e) = Utils.concatStrings (strExpr e)

    (* Prints the Tiger program *)
    (* print : Tiger.Prog -> unit *)
    and print p = Utils.printOut (prettyTig p)
end
