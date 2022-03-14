(* Signature for the AST of the Tiger Language *)
signature TIGER =
sig

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

                (* For Loops *)
                (* 'for' id ':=' exp 'to' exp 'do' exp 'done' *)
                (* 'for' id ':=' exp 'to' exp 'by' exp 'do' exp 'done' *)
                | For of {loopVar: id, startPos: Expr, endPos: Expr, step: Expr, body: Expr}

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

    (* Utility Functions *)
    val getOpRec : Expr -> BinOp -> Expr -> {left: Expr, oper: BinOp, right: Expr}

    val getAsignRec : Lvalue -> Expr -> {lvalue : Lvalue, expr : Expr}

    val getForRec : id -> Expr -> Expr -> Expr -> Expr ->
                            {loopVar : id, startPos : Expr, endPos : Expr, step : Expr, body : Expr}
end
