(* Structure for the AST of the Tiger Language *)
structure Tiger :> TIGER =
struct

    (* The id's used for identification of variable names *)
    type id = string

    (* Expressions datatype *)
    datatype Expr =
                (* Literals *)
                  Nil
                | Int of int

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

    (* Utility functions *)
    fun getOpRec left oper right = {left = left, oper = oper, right = right}
end
