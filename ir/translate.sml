(* Structure to translate Tiger.Prog to Ir.Prog and then to Mips.Prog *)
signature TRANSLATE =
sig
    val compileToIR   : Env.mp -> Tiger.Prog -> Ir.Prog * Env.mp
    val compileToMips : Ir.Prog -> (string, Mips.Reg) Mips.Prog
end

structure Translate :> TRANSLATE =
struct

    structure TIG = Tiger
    structure PTA = PrintTigerAST
    structure CTM = ConvToMIPS

    (* The result type of intermediate expressions *)
    datatype Result = IntRes  of int
                    | TempRes of Temp.value

    (* Raising exception and printing the error message *)
    exception UnsupportedOperation of string
    exception NotDefined of string

    (* raiseUnsupportedOperationException : string -> Tiger.Expr *)
    fun raiseUnsupportedOperationException msg =
                    Utils.throwErr UnsupportedOperation ("[translate.sml]:" ^ msg)

    fun raiseNotDefinedException msg =
                    Utils.throwErr NotDefined ("[translate.sml]:" ^ msg)

    (* Assign a temporary value to the string if not already there *)
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

    (* Get the temporary value allocated to the Tiger.Lvalue *)
    (* getTemp : Env.mp -> string -> Temp.value *)
    fun getTemp (env : Env.mp) (id: string) =
            (case Env.find env id of
                  SOME x  => x
                | NONE    => raiseNotDefinedException ("[getTemp]: Undefined variable " ^ id)
            )

    (* Simplifies the nested expression into resultant expression *)
    (* evalExpr : Env.mp -> Tiger.Expr
                                -> Result * Env.mp * Ir.Inst list *)
    fun evalExpr env (TIG.Int i)   = (IntRes i, env, [])
      | evalExpr env (TIG.Lval l)  = evalLvalueExpr env l
      | evalExpr env (TIG.Op r)    = evalOpExpr env (#left r) (#oper r) (#right r)
      | evalExpr env (TIG.Neg e)   = evalNegExpr env e
      | evalExpr env (TIG.Exprs e) = evalExprs env e
      | evalExpr env  e            = raiseUnsupportedOperationException ("[evalExpr]: Operation not supported:\n" ^ (PTA.prettyTig (TIG.Expression e)))

    (* Simplifies the nested lvalue expression into resultant expression *)
    (* evalLvalueExpr : Env.mp -> Tiger.Lvalue
                                        -> Result * Env.mp * Ir.Inst list *)
    and evalLvalueExpr env (TIG.Var id) = (TempRes (getTemp env id), env, [])

    (* Simplifies nested binary operator expression *)
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

    (* Simplifies nested binary operator expression of the type (x := a + b) *)
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

    (* Simplifies the nested negation expression *)
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

    (* Simplifies the nested negation expression of the type (x := ~y) *)
    (* evalReducedNegExpr : Temp.value -> Result
                                            -> Ir.Inst list *)
    and evalReducedNegExpr (t: Temp.value) res = case res of
                                  IntRes i  => [CTM.mLi  t (~i) CTM.DUMMY_STR]
                                | TempRes v => [CTM.mNeg t v    CTM.DUMMY_STR]

   (* Simplifies the list of nested negation expressions *)
    (* evalExprs : Env.mp -> Tiger.Expr list
                                -> Result * Env.mp * Ir.Inst list *)
    and evalExprs env []  = raiseUnsupportedOperationException ("[evalExprs]: Operation not supported:\n" ^ (PTA.prettyTig (TIG.Expression (TIG.Exprs []))))
      | evalExprs env [e] = evalExpr env e
      | evalExprs env (e :: es) = let
                                      val (res, newEnv1, irProg) = evalExpr env e
                                      val (res2, newEnv2, irProg2) = evalExprs newEnv1 es
                                  in
                                      (res2, newEnv2, irProg @ irProg2)
                                  end

    (* Translates the Tiger.Expr to Ir.Inst list *)
    (* translateExpr : Env.mp -> Tiger.Expr
                                    -> Ir.Inst list * Env.mp *)
    and translateExpr env (TIG.Assign e) = assignExprHelper env (#lvalue e) (#expr e)
      | translateExpr env (TIG.Print e)  = printExprHelper env e
      | translateExpr env (TIG.Exprs e)  = translateExprs env e
      | translateExpr env _              = ([], env)

    (* Translates the Tiger.Expr list to Ir.Inst list *)
    (* tranlateExprs : Env.mp -> Tiger.Expr list
                                        -> Ir.Inst list * Env.mp *)
    and translateExprs env []        = ([], env)
      | translateExprs env (e :: es) = let
                                            val (prog1, env1) = translateExpr env e
                                            val (prog2, env2) = translateExprs env1 es
                                       in
                                            (prog1 @ prog2, env2)
                                       end

    (* Utility function manager for translating Tiger.Assign expressions *)
    (* assignExprHelper : Env.mp -> Tiger.Lvalue -> Tiger.Expr
                                                         -> Ir.Inst list * Env.mp *)
    and assignExprHelper env lval expr =
            let
                val (newEnv, t) = (case lval of (* Assigning temporary value for lval *)
                                    TIG.Var v => assignTemp env v)
            in
                (case expr of
                      TIG.Int i  => ([CTM.mLi t i CTM.DUMMY_STR], newEnv)
                    | TIG.Lval l => (case l of
                            TIG.Var id => ([CTM.mMove t (getTemp env id) CTM.DUMMY_STR], newEnv)
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
                    | TIG.Exprs e =>
                        let
                            val (res, newEnv1, prog) = evalExprs env e
                            val addProg              = assignExprSimplified t res
                            val resEnv               = Env.union newEnv1 newEnv
                        in
                            (prog @ addProg, resEnv)
                        end
                    | e           => raiseUnsupportedOperationException ("[assignExprHelper]: Operation not supported:\n" ^ (PTA.prettyTig (TIG.Expression e)))
                )
            end

    (* For simplified Tiger.Assign expression of the form lvalue := result *)
    (* assignExprSimplified : Temp.value -> Result
                                                -> Ir.Inst list *)
    and assignExprSimplified (t: Temp.value) (res: Result) = case res of
                          IntRes i  => [CTM.mLi   t i CTM.DUMMY_STR]
                        | TempRes v => [CTM.mMove t v CTM.DUMMY_STR]

    (* Utility function manager for translating Tiger.Print expressions *)
    (* printExprHelper : Env.mp -> Tiger.Expr
                                        -> Ir.Inst list * Env.mp *)
    and printExprHelper env expr =
            let
                val (aEnv, a0) = assignTemp env Utils.A0_REG  (* For register a0 *)
                val _          = RegAlloc.allocSpecialReg a0 Mips.A0

                val (newEnv, t) = assignTemp aEnv Utils.V0_REG  (* For register v0 *)
                val _           = RegAlloc.allocSpecialReg t Mips.V0

                (* Instruction set for printing *)
                val printInst = CTM.mSyscall t Utils.PRINT_INT_SYSCALL CTM.DUMMY_STR
            in
                (case expr of
                      TIG.Int i  => ([CTM.mLi a0 i CTM.DUMMY_STR] @ printInst, newEnv)
                    | TIG.Lval l => (case l of
                            TIG.Var id =>
                                ([CTM.mMove a0 (getTemp env id) CTM.DUMMY_STR] @ printInst, newEnv)
                        )
                    | TIG.Op r   =>
                        let
                            val {left, oper, right}    = r
                            val (lRes, newEnv1, lProg) = evalExpr env left
                            val (rRes, newEnv2, rProg) = evalExpr env right
                            val newEnv3                = Env.union newEnv1 newEnv2
                            val prog                   = evalReducedOpExpr a0 lRes oper rRes
                            val resEnv                 = Env.union newEnv3 newEnv
                        in
                            (lProg @ rProg @ prog @ printInst, resEnv)
                        end
                    | TIG.Neg e  =>
                        let
                            val (res, newEnv1, irProg) = evalExpr env e
                            val addProg                = evalReducedNegExpr a0 res
                            val resEnv                 = Env.union newEnv1 newEnv
                        in
                            (irProg @ addProg @ printInst, resEnv)
                        end
                    | TIG.Exprs e =>
                        let
                            val (res, newEnv1, prog) = evalExprs env e
                            val addProg              = assignExprSimplified a0 res
                            val resEnv               = Env.union newEnv1 newEnv
                        in
                            (prog @ addProg @ printInst, resEnv)
                        end
                    | e                   => raiseUnsupportedOperationException ("[printExprHelper]: Operation not supported:\n" ^ (PTA.prettyTig (TIG.Expression e)))
                )
            end

    (* Compiles Tiger program to Ir *)
    (* compileToIR : Env.mp -> Tiger.Prog -> Ir.Prog * Env.mp *)
    and compileToIR env (TIG.Expression e) =
            let
                val (instList, newEnv) = translateExpr env e
                val stmtList        = map (CTM.mapInstToStmt) instList

                val headerDirs  = [Mips.Data, Mips.Text, Mips.Globl "main"]
                val headerStmts = map (CTM.mapDirToStmt CTM.DUMMY_STR Temp.DUMMY_VALUE) headerDirs

                val exitInsts = CTM.mSyscall (getTemp newEnv Utils.V0_REG) Utils.EXIT_SYSCALL CTM.DUMMY_STR
                val exitStmt = map (CTM.mapInstToStmt) exitInsts
            in
                (headerStmts @ [Mips.Label "main"] @ stmtList @ exitStmt, newEnv)
            end

    (* Compiles Ir program to MIPS *)
    (* compileToMips : Ir.Prog -> (string, Mips.Reg) Mips.Prog *)
    and compileToMips prog = Mips.mapProg Utils.identity RegAlloc.getReg prog
end
