(* structure for the AST of the Tiger Language *)

structure Tiger =
struct

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

        (* Binary Operators *)
        and BinOp = Plus
                  | Minus
                  | Mul
                  | Div

        (* Name of variables *)
        and Lvalue = Var of id

    (* Program Datatype *)
    datatype Prog = Expression of Expr

    (* Some utility functions *)
    fun getOpRec left oper right = {left = left, oper = oper, right = right}

    (*===========================================================================*)
    (* Conversion functions *)

    (* Convert all binary operations to Tiger AST *)
    fun convertPlus  a b = Op (getOpRec a Plus b)
    fun convertMinus a b = Op (getOpRec a Minus b)
    fun convertMul   a b = Op (getOpRec a Mul b)
    fun convertDiv   a b = Op (getOpRec a Div b)

    (* Convert to Assignment Instruction *)
    fun convertAssign l e = Assign {lvalue = l, expr = e}
end
