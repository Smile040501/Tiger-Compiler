(* Pretty Prints the AST of the Tiger Language *)
signature PRETTY_TIGER_AST =
sig
    val prettyTig : Tiger.Prog -> string
end

(* Structure to pretty print the AST generated for the Tiger Language *)
structure PrettyTigerAST :> PRETTY_TIGER_AST =
struct
    open Tiger

    val indent = Utils.indent   (* Indentation function *)

    (* Convert Expr datatype to string representation *)
    (* strExpr : Tiger.Expr -> int -> int -> string *)
    (* is : indentation that has to be applied on the first line string of the stringified form *)
    (* il : indentation that should be actually be applied on the first line string ideally *)
    fun strExpr Nil            is _   = indent "Nil" is
      | strExpr (Int n)        is _   = indent ("Int(" ^ (Int.toString n) ^ ")") is
      | strExpr (Lval l)       is _   = indent ("Lval(" ^ (strLvalue l) ^ ")") is
      | strExpr (Op operation) is il  =
                let
                    val {left, oper, right} = operation
                    val strLeft  = strExpr  left  0 (il + 12) (* il + 4 + cnt('left  = ') *)
                    val strOper  = strBinOp oper
                    val strRight = strExpr  right 0 (il + 12) (* il + 4 + cnt('right = ') *)
                in
                    (indent "Op({\n" is) ^ (indent ("left  = " ^ strLeft ^ ",\n") (il + 4)) ^
                    (indent ("oper  = " ^ strOper ^ ",\n") (il + 4)) ^
                    (indent ("right = " ^ strRight ^ "\n") (il + 4)) ^
                    (indent "})" il)
                end
      | strExpr (Neg e)        is il  = indent ("Neg(" ^ (strExpr e 0 (il + 5)) ^ ")") is
      | strExpr (Assign r)     is il  =
                let
                    val {lvalue, expr} = r
                    val strLval = strLvalue lvalue
                    val strE    = strExpr expr 0 (il + 13) (* il + 4 + cnt('expr   = ') *)
                in
                    (indent "Assign({\n" is) ^
                    (indent ("lvalue = " ^ strLval ^ ",\n") (il + 4)) ^
                    (indent ("expr   = " ^ strE ^ "\n") (il + 4)) ^
                    (indent "})" il)
                end
      | strExpr (Print e)      is il  = indent ("Print(" ^ (strExpr e 0 (il + 7)) ^ ")") is
      | strExpr (Exprs es)     is il  = ((indent "Exprs([\n" is) ^
                                        (strExprs es (il + 4) (il + 4)) ^
                                        (indent "])" il))

    (* Converts List of Expr datatype to string *)
    (* strExprs : Tiger.Expr list -> int -> int -> string *)
    and strExprs []        _  _  = ""
      | strExprs (x :: xs) is il =
                (case xs of
                    [] => (strExpr x is il) ^ "\n"
                    | _ => (strExpr x is il) ^ ",\n" ^ (strExprs xs is il)
                )

    (* Converts BinOp datatype to string *)
    (* strExpr : Tiger.BinOp -> string *)
    and strBinOp Plus  = "Plus"
      | strBinOp Minus = "Minus"
      | strBinOp Mul   = "Mul"
      | strBinOp Div   = "Div"

    (* Converts Lvalue datatype to strings *)
    (* strLvalue : Tiger.Lvalue -> string *)
    and strLvalue (Var x) = "Var(" ^ x ^ ")"

    (* Returns the string representation of the Tiger AST *)
    (* prettyTig : Tiger.Prog -> string *)
    and prettyTig (Expression e) = strExpr e 0 0
end
