(* signature for the AST of the Tiger Language *)

signature TIGER =
sig

    (* The id's used for identification of variable names *)
    type id = string

    (* Expressions datatype *)
    datatype Expr =
                (* Literals *)
                  Int of int

                (* Variables *)
                | Lval of Lvalue

                (* Operations *)
                | Op  of {left: Expr, oper: BinOp, right: Expr}
                | Neg of Expr

                (* Assignment *)
                (* lvalue := exp *)
                | Assign of {lvalue: Lvalue, expr: Expr}

                (* Print *)
                | Print of Expr

                (* Multiple expressions *)
                | Exprs of Expr list

        (* Binary Operators *)
        and BinOp = Plus
                  | Minus
                  | Mul
                  | Div

        (* Name of variables *)
        and Lvalue = Var of id

    (* Program Datatype *)
    datatype Prog = Expression of Expr

    (*===========================================================================*)
    (* Conversion functions *)

    (* Convert all binary operations to Tiger AST *)
    val convertPlus : Expr -> Expr -> Expr
    val convertMinus: Expr -> Expr -> Expr
    val convertMul  : Expr -> Expr -> Expr
    val convertDiv  : Expr -> Expr -> Expr

    (* Convert to Assignment Instruction *)
    val convertAssign: Lvalue -> Expr -> Expr
end
