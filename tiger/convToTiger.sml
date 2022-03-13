(* Contains utility functions to create a Tiger AST Expression *)
signature CONV_TO_TIGER =
sig
    val tPlus : Tiger.Expr -> Tiger.Expr -> Tiger.Expr
    val tMinus: Tiger.Expr -> Tiger.Expr -> Tiger.Expr
    val tMul  : Tiger.Expr -> Tiger.Expr -> Tiger.Expr
    val tDiv  : Tiger.Expr -> Tiger.Expr -> Tiger.Expr

    val tAssign: Tiger.Lvalue -> Tiger.Expr -> Tiger.Expr
end

structure ConvToTiger :> CONV_TO_TIGER =
struct
    open Tiger

    (* Conversion functions *)

    (* Convert all binary operations to Tiger AST *)
    fun tPlus  a b = Op (getOpRec a Plus b)
    fun tMinus a b = Op (getOpRec a Minus b)
    fun tMul   a b = Op (getOpRec a Mul b)
    fun tDiv   a b = Op (getOpRec a Div b)

    (* Convert to Assignment Instruction *)
    fun tAssign l e = Assign {lvalue = l, expr = e}
end
