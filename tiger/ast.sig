(* Signature for the AST of the Tiger Language *)
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
end
