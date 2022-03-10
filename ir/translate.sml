signature TRANSLATE =
sig
    val compileToIR : Env.mp -> Tiger.Prog -> Ir.Prog * Env.mp
end

structure Translate :> TRANSLATE =
struct
    exception UnsupportedOperation of string
    exception NotDefined of string

    structure TIG = Tiger
    structure PTA = PrintTigerAST
    structure CTM = ConvToMIPS

    (* The result type of intermediate expressions *)
    datatype Result = IntRes  of int
                    | TempRes of Temp.value

    fun raiseUnsupportedOperationException str = (
        TextIO.output(TextIO.stdErr, str);
        raise UnsupportedOperation "Operation not supported"
    )

    fun raiseNotDefinedException str = (
        TextIO.output(TextIO.stdErr, str);
        raise NotDefined "Undefined variable"
    )

    (* assignTemp : Env.mp -> string -> Env.mp * Temp.value *)
    fun assignTemp (env : Env.mp) (id: string) =
            (case Env.find env id of
                  SOME t => (env, t)
                | NONE   => let
                                val t      = Temp.newValue ()
                                val newEnv = Env.insert env id t
                            in
                                (newEnv, t)
                            end
            )

    (* getTemp : Env.mp -> Tiger.Lvalue -> Temp.value *)
    fun getTemp (env : Env.mp) (TIG.Var id) =
            (case Env.find env id of
                  SOME (x: Temp.value)  => x
                | NONE    => raiseNotDefinedException ("traslate.sml:getTemp Undefined variable " ^ id ^ "\n")
            )

    (* Simplifies the nested expression into resultant expression *)
    (* evalExpr : Env.mp -> Tiger.Expr
                                -> Result * Env.mp * Ir.Inst list *)
    fun evalExpr env (TIG.Int i)  = (IntRes i, env, [])
      | evalExpr env (TIG.Lval l) = (TempRes (getTemp env l), env, [])
      | evalExpr env (TIG.Op r)   = evalOpExpr env (#left r) (#oper r) (#right r)
      | evalExpr env (TIG.Neg e)  = evalNegExpr env e
      | evalExpr env  e           = raiseUnsupportedOperationException ("translate.sml:evalExpr Operation not supported:\n" ^ (PTA.getStr (TIG.Expression e)) ^ "\n")

    (* evalOpExpr : Env.mp -> Tiger.Expr -> Tiger.BinOp -> Tiger.Expr
                                                                -> Result * Env.mp * Ir.Inst list *)
    and evalOpExpr env left oper right =
            let
                val (lRes, newEnv1, lProg) = evalExpr env left
                val (rRes, newEnv2, rProg) = evalExpr env right
                val newEnv3                = Env.union newEnv1 newEnv2
                val tempLabel              = Temp.newLabel ()
                val (newEnv4, t)           = assignTemp newEnv3 tempLabel
                val addProg                = evalReducedOpExpr t lRes oper rRes
            in
                (TempRes t, newEnv4, lProg @ rProg @ addProg)
            end

    (* evalNegExpr : Env.mp -> Tiger.Expr
                                    -> Result * Env.mp * Ir.Inst list *)
    and evalNegExpr env e =
            let
                val (res, newEnv1, irProg) = evalExpr env e
                val tempLabel              = Temp.newLabel ()
                val (newEnv2, t)           = assignTemp newEnv1 tempLabel
                val addProg                = evalReducedNegExpr t res
            in
                (TempRes t, newEnv2, irProg @ addProg)
            end

    (* evalReducedOpExpr : Temp.value -> Result -> Tiger.BinOp -> Result
                                                                    -> Ir.Inst list *)
    and evalReducedOpExpr (t: Temp.value) lRes TIG.Plus rRes =
            (case (lRes, rRes) of
                  (IntRes i, IntRes j)   => [CTM.mLi t i CTM.DUMMY_STR, CTM.mAddi t t j CTM.DUMMY_STR]
                | (IntRes i, TempRes j)  => [CTM.mAddi t j i CTM.DUMMY_STR]
                | (TempRes i, IntRes j)  => [CTM.mAddi t i j CTM.DUMMY_STR]
                | (TempRes i, TempRes j) => [CTM.mAdd  t i j CTM.DUMMY_STR]
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Minus rRes =
            (case (lRes, rRes) of
                  (IntRes i, IntRes j)   => [CTM.mLi t i CTM.DUMMY_STR, CTM.mSub_I t t j CTM.DUMMY_STR]
                | (IntRes i, TempRes j)  => [CTM.mSub_I t j i CTM.DUMMY_STR]
                | (TempRes i, IntRes j)  => [CTM.mSub_I t i j CTM.DUMMY_STR]
                | (TempRes i, TempRes j) => [CTM.mSub   t i j CTM.DUMMY_STR]
            )
    | evalReducedOpExpr (t: Temp.value) lRes TIG.Mul rRes =
            (case (lRes, rRes) of
                  (IntRes i, IntRes j)   => [CTM.mLi t i CTM.DUMMY_STR, CTM.mMul_I t t j CTM.DUMMY_STR]
                | (IntRes i, TempRes j)  => [CTM.mMul_I t j i CTM.DUMMY_STR]
                | (TempRes i, IntRes j)  => [CTM.mMul_I t i j CTM.DUMMY_STR]
                | (TempRes i, TempRes j) => [CTM.mMul   t i j CTM.DUMMY_STR]
            )
    | evalReducedOpExpr (t: Temp.value) lRes TIG.Div rRes =
            (case (lRes, rRes) of
                  (IntRes i, IntRes j)   => [CTM.mLi t i CTM.DUMMY_STR, CTM.mDiv_QI t t j CTM.DUMMY_STR]
                | (IntRes i, TempRes j)  => [CTM.mDiv_QI t j i CTM.DUMMY_STR]
                | (TempRes i, IntRes j)  => [CTM.mDiv_QI t i j CTM.DUMMY_STR]
                | (TempRes i, TempRes j) => [CTM.mDiv_Q  t i j CTM.DUMMY_STR]
            )

    (* evalReducedNegExpr : Temp.value -> Result
                                            -> Ir.Inst list *)
    and evalReducedNegExpr (t: Temp.value) res = case res of
                                  IntRes i  => [CTM.mLi  t (~i) CTM.DUMMY_STR]
                                | TempRes v => [CTM.mNeg t v    CTM.DUMMY_STR]

    (* translateExpr : Env.mp -> Tiger.Expr
                                    -> Ir.Inst list * Env.mp *)
    and translateExpr env (TIG.Assign e) = assignExprHelper env (#lvalue e) (#expr e)
      | translateExpr env (TIG.Print e)  = printExprHelper env e
      | translateExpr env (TIG.Exprs e)  = translateExprs env e
      | translateExpr env _              = ([], env)

    (* tranlateExprs : Env.mp -> Tiger.Expr list
                                        -> Ir.Inst list * Env.mp *)
    and translateExprs env []        = ([], env)
      | translateExprs env (e :: es) = let
                                            val (prog1, env1) = translateExpr env e
                                            val (prog2, env2) = translateExprs env1 es
                                       in
                                            (prog1 @ prog2, env2)
                                       end

    (* assignExprHelper : Env.mp -> Tiger.Lvalue -> Tiger.Expr
                                                         -> Ir.Inst list * Env.mp *)
    and assignExprHelper env lval expr =
            let
                val (newEnv, t) = (case lval of
                                    TIG.Var v => assignTemp env v)
            in
                (case expr of
                      TIG.Int i  => ([CTM.mLi t i CTM.DUMMY_STR], newEnv)
                    | TIG.Lval l => (case l of
                            TIG.Var id => ([CTM.mMove t (getTemp env l) CTM.DUMMY_STR], newEnv)
                        )
                    | TIG.Op r   =>
                        let
                            val {left, oper, right}    = r
                            val (lRes, newEnv1, lProg) = evalExpr env left
                            val (rRes, newEnv2, rProg) = evalExpr env right
                            val newEnv3                = Env.union newEnv1 newEnv2
                            val prog                   = evalReducedOpExpr t lRes oper rRes
                            val resEnv                 = Env.union newEnv3 newEnv
                        in
                            (lProg @ rProg @ prog, resEnv)
                        end
                    | TIG.Neg e  =>
                        let
                            val (res, newEnv1, irProg) = evalExpr env e
                            val addProg                = evalReducedNegExpr t res
                            val resEnv                 = Env.union newEnv1 newEnv
                        in
                            (irProg @ addProg, resEnv)
                        end
                    | _                   => raiseUnsupportedOperationException "translate.sml:assignExprHelper"
                )
            end

    (* printExprHelper : Env.mp -> Tiger.Expr
                                        -> Ir.Inst list * Env.mp *)
    and printExprHelper env expr = ([], env)

    (* Compiles Tiger program to Ir *)
    (* compileToIR : Env.mp -> Tiger.Prog -> Ir.Prog * Env.mp *)
    and compileToIR env (TIG.Expression e) =
            let
                val (instList, env) = translateExpr env e
                val stmtList = map (CTM.mapInstToStmt) instList
                val headerDirs = [Mips.Data, Mips.Text, Mips.Globl "main"] (* Todo add main label *)
                val headerStmts = map (CTM.mapDirToStmt CTM.DUMMY_STR Temp.DUMMY_VALUE) headerDirs
            in
                (headerStmts @ [Mips.Label "main"] @ stmtList, env)
            end
end
