(* Structure to translate Tiger.Prog to Ir.Prog and then to Mips.Prog *)
signature TRANSLATE =
sig
    val temps         : (string * Temp.value) list ref
    val compileToIR   : Tiger.Prog -> Env.mp list * Ir.Prog
    val compileToMips : Ir.Prog -> (string, Mips.Reg) Mips.Prog
end

structure Translate :> TRANSLATE =
struct

    structure TIG = Tiger
    structure PTA = PrettyTigerAST
    structure CTM = ConvToMIPS
    structure RA  = RegAlloc

    val updateFirstVal = Utils.updateFirstVal
    val updateLastVal  = Utils.updateLastVal
    val getLastVal     = Utils.getLastVal

    (* The result type of intermediate (nested) expressions *)
    datatype Result = IntRes  of int
                    | TempRes of Temp.value

    (* Raising exception and printing the error message *)
    exception UnsupportedOperation of string
    exception NotDefined of string

    (* raiseUnsupportedOperationException : string -> Tiger.Expr *)
    fun raiseUnsupportedOperationException msg =
                    Utils.throwErr UnsupportedOperation ("[translate.sml]:" ^ msg ^ "\n\n")

    fun raiseNotDefinedException msg =
                    Utils.throwErr NotDefined ("[translate.sml]:" ^ msg ^ "\n\n")

    (* The temporaries allocation performed by the compiler *)
    (* val temps : (string * Temp.value) list ref *)
    val temps : (string * Temp.value) list ref = ref []

    (* Assign a temporary value to the string if not already there.
       Allocates a new register if new temporary value is created and being mentioned to do so. *)
    (* assignTemp : Env.mp -> string -> bool -> Env.mp * Temp.value *)
    fun assignTemp (env : Env.mp) (id: string) (allocateReg: bool) =
            (case Env.find env id of
                  SOME t => (env, t)
                | NONE   => let
                                val t      = Temp.newValue ()
                                val newEnv = Env.insert env id t
                                val _      = temps := (!temps @ [(id, t)])
                                val _      = if allocateReg then (RA.allocReg t)
                                             else ()
                            in
                                (newEnv, t)
                            end
            )

    (* Get the temporary value allocated to the Tiger.Lvalue *)
    (* getTemp : Env.mp list -> string -> Temp.value *)
    fun getTemp (envs : Env.mp list) (id: string) = case envs of
              [] => raiseNotDefinedException ("[getTemp]: Undefined variable " ^ id)
            | (e :: es) => (case Env.find e id of
                                  SOME t => t
                                | NONE   => getTemp es id
                            )

    (* Simplifies the nested expression into resultant expression *)
    (* evalExpr : Env.mp list -> Tiger.Expr
                        -> Result * Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    (* The first list is the list of statements to be expected in the MIPS main program.
       The second list is the list of statements to be expected in the MIPS data section. *)
    fun evalExpr (envs : Env.mp list) (exp : TIG.Expr) =
            (case exp of
                  (TIG.Int   i) => (IntRes i, envs, [], [])
                | (TIG.Lval  l) => evalLvalueExpr envs l
                | (TIG.Op    r) => evalOpExpr     envs (#left r) (#oper r) (#right r)
                | (TIG.Neg   e) => evalNegExpr    envs e
                | (TIG.Exprs e) => evalExprs      envs e
                |  e            => (raiseUnsupportedOperationException
                                        ("[evalExpr]: Operation not supported:\n" ^
                                            (PTA.prettyTig (TIG.Expression e)))
                                    )
            )

    (* Simplifies the nested lvalue expression into resultant expression *)
    (* evalLvalueExpr : Env.mp list -> Tiger.Lvalue
                                -> Result * Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and evalLvalueExpr envs (TIG.Var id) = (TempRes (getTemp envs id), envs, [], [])

    (* Simplifies nested binary operator expression *)
    (* evalOpExpr : Env.mp list -> Tiger.Expr -> Tiger.BinOp -> Tiger.Expr
                                        -> Result * Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and evalOpExpr envs left oper right =
            let
                val (lRes, newEnvs1, lInsts, lData) = evalExpr envs left
                val (rRes, newEnvs2, rInsts, rData) = evalExpr envs right
                val newEnv3      = Env.union (List.hd newEnvs1) (List.hd newEnvs2)
                val tempLabel    = Temp.newLabel ()
                val (newEnv4, t) = assignTemp newEnv3 tempLabel true
                val (addInsts, addData) = evalReducedOpExpr t lRes oper rRes
            in
                (TempRes t, updateFirstVal envs newEnv4,
                            lInsts @ rInsts @ addInsts, lData @ rData @ addData)
            end

    (* Simplifies nested binary operator expression of the type (x := a + b) *)
    (* evalReducedOpExpr : Temp.value -> Result -> Tiger.BinOp -> Result
                                                        -> Ir.Stmt list * Ir.Stmt list *)
    and evalReducedOpExpr (t: Temp.value) lRes TIG.Plus rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR, CTM.mAddi t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mAddi t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mAddi t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mAdd  t i j CTM.DUMMY_STR], [])
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Minus rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR, CTM.mSub_I t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mSub_I t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mSub_I t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mSub   t i j CTM.DUMMY_STR], [])
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Mul rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR, CTM.mMul_I t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mMul_I t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mMul_I t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mMul   t i j CTM.DUMMY_STR], [])
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Div rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR, CTM.mDiv_QI t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mDiv_QI t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mDiv_QI t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mDiv_Q  t i j CTM.DUMMY_STR], [])
            )

    (* Simplifies the nested negation expression *)
    (* evalNegExpr : Env.mp list -> Tiger.Expr
                                    -> Result * Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and evalNegExpr envs e =
            let
                val (res, newEnvs, insts, data) = evalExpr envs e
                val tempLabel   = Temp.newLabel ()
                val (newEnv, t) = assignTemp (List.hd newEnvs) tempLabel true
                val (addInsts, addData) = evalReducedNegExpr t res
            in
                (TempRes t, updateFirstVal envs newEnv, insts @ addInsts, data @ addData)
            end

    (* Simplifies the nested negation expression of the type (x := ~y) *)
    (* evalReducedNegExpr : Temp.value -> Result
                                            -> Ir.Stmt list * Ir.Stmt list *)
    and evalReducedNegExpr (t: Temp.value) res =
            (case res of
                  IntRes  i => ([CTM.mLi  t (~i) CTM.DUMMY_STR], [])
                | TempRes v => ([CTM.mNeg t v    CTM.DUMMY_STR], [])
            )

   (* Simplifies the list of nested expressions *)
    (* evalExprs : Env.mp list -> Tiger.Expr list
                                    -> Result * Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and evalExprs (envs: Env.mp list) (exprs: TIG.Expr list) =
            (case exprs of
                  []        => (raiseUnsupportedOperationException
                                    ("[evalExprs]: Operation not supported:\n" ^
                                            (PTA.prettyTig (TIG.Expression (TIG.Exprs exprs))))
                                )
                | [e]       => evalExpr envs e
                | (e :: es) => let
                                   val (res, newEnvs1, insts1, data1)  = evalExpr envs e
                                   val (res2, newEnvs2, insts2, data2) = evalExprs newEnvs1 es
                               in
                                   (res2, newEnvs2, insts1 @ insts2, data1 @ data2)
                               end
            )

    (* Translates the Tiger.Expr to Ir.Stmt list *)
    (* translateExpr : Env.mp list -> Tiger.Expr
                                    -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and translateExpr (envs: Env.mp list) (exp: TIG.Expr) =
            (case exp of
                  (TIG.Assign e) => assignExprHelper envs (#lvalue e) (#expr e)
                | (TIG.For    e) => (forExprHelper envs
                                        (#loopVar e) (#startPos e) (#endPos e) (#step e) (#body e)
                                    )
                | (TIG.Print  e) => printExprHelper envs e
                | (TIG.Exprs  e) => translateExprs envs e
                | _              => (envs, [], [])
            )

    (* Translates the Tiger.Expr list to Ir.Stmt list *)
    (* tranlateExprs : Env.mp list -> Tiger.Expr list
                                        -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and translateExprs (envs: Env.mp list) (exprs: TIG.Expr list) =
            (case exprs of
                  []        => (envs, [], [])
                | (e :: es) => let
                                   val (newEnvs1, insts1, data1) = translateExpr envs e
                                   val (newEnvs2, insts2, data2) = translateExprs newEnvs1 es
                               in
                                   (newEnvs2, insts1 @ insts2, data1 @ data2)
                               end
            )

    (* Utility function manager for translating Tiger.Assign expressions *)
    (* assignExprHelper : Env.mp list -> Tiger.Lvalue -> Tiger.Expr
                                                -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and assignExprHelper envs lval expr =
            let
                val (newEnv, t) = (case lval of (* Assigning temporary value for lval *)
                                        TIG.Var v => assignTemp (List.hd envs) v true
                                    )
            in
                (case expr of
                      TIG.Int i  => (updateFirstVal envs newEnv, [CTM.mLi t i CTM.DUMMY_STR], [])
                    | TIG.Lval l => (case l of
                                        TIG.Var id => (updateFirstVal envs newEnv,
                                                        [CTM.mMove t (getTemp envs id) CTM.DUMMY_STR], []
                                                       )
                                    )
                    | TIG.Op r   =>
                        let
                            val {left, oper, right} = r
                            val (lRes, newEnvs1, lInsts, lData) = evalExpr envs left
                            val (rRes, newEnvs2, rInsts, rData) = evalExpr envs right
                            val (addInsts, addData) = evalReducedOpExpr t lRes oper rRes
                            val newEnv3             = Env.union (List.hd newEnvs1) (List.hd newEnvs2)
                            val resEnv              = Env.union newEnv3 newEnv
                        in
                            (updateFirstVal envs resEnv,
                                    lInsts @ rInsts @ addInsts, lData @ rData @ addData)
                        end
                    | TIG.Neg e  =>
                        let
                            val (res, newEnvs, insts, data) = evalExpr envs e
                            val (addInsts, addData) = evalReducedNegExpr t res
                            val resEnv              = Env.union (List.hd newEnvs) newEnv
                        in
                            (updateFirstVal envs resEnv, insts @ addInsts, data @ addData)
                        end
                    | TIG.Exprs e =>
                        let
                            val (res, newEnvs, insts, data) = evalExprs envs e
                            val (addInsts, addData) = assignExprSimplified t res
                            val resEnv              = Env.union (List.hd newEnvs) newEnv
                        in
                            (updateFirstVal envs resEnv, insts @ addInsts, data @ addData)
                        end
                    | e           => (raiseUnsupportedOperationException
                                            ("[assignExprHelper]: Operation not supported:\n" ^
                                                    (PTA.prettyTig (TIG.Expression e)))
                                        )
                )
            end

    (* For simplified Tiger.Assign expression of the form lvalue := result *)
    (* assignExprSimplified : Temp.value -> Result
                                        -> Ir.Stmt list * Ir.Stmt list *)
    and assignExprSimplified (t: Temp.value) (res: Result) =
            (case res of
                  IntRes  i => ([CTM.mLi   t i CTM.DUMMY_STR], [])
                | TempRes v => ([CTM.mMove t v CTM.DUMMY_STR], [])
            )

    (* forExprHelper : Env.mp list -> Tiger.id -> Tiger.Expr -> Tiger.Expr -> Tiger.Expr -> Tiger.Expr
                                            -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and forExprHelper envs loopVar startPos endPos step body =
            let
                val forLoop       = Temp.newLabel ()        (* Unique label for the loop  *)
                val loopStart     = "forStart" ^ forLoop    (* Start of the loop          *)
                val loopCondition = "forCond"  ^ forLoop    (* Condition for the loop     *)
                val gtCondition   = "forGT"    ^ forLoop    (* To be used if start <= end *)
                val ltCondition   = "forLT"    ^ forLoop    (* To be used if start > end  *)
                val loopBody      = "forBody"  ^ forLoop    (* Body of the loop           *)
                val loopStep      = "forStep"  ^ forLoop    (* Step of the loop           *)
                val loopEnd       = "forEnd"   ^ forLoop    (* End of the loop            *)

                val (newEnv, t) = assignTemp (Env.empty ()) loopVar true
                val (startRes, startEnvs, startInsts, startData) = evalExpr envs startPos
                val (endRes,   endEnvs,   endInsts,   endData  ) = evalExpr envs endPos
                val (stepRes,  stepEnvs,  stepInsts,  stepData ) = evalExpr envs step
                val (bodyEnvs, bodyInsts, bodyData) = translateExpr (newEnv :: envs) body

                val newEnv2 = Env.union (List.hd startEnvs) (List.hd endEnvs)
                val newEnv3 = Env.union newEnv2 (List.hd stepEnvs)
                val newEnvs4 = updateFirstVal envs newEnv3
                val newEnvs5 = updateLastVal newEnvs4 (getLastVal bodyEnvs)

                val (resEnvs, nt) = case (startRes, endRes) of
                      (IntRes _, IntRes _) =>
                                    let
                                        val iLabel = Temp.newLabel ()
                                        val (iEnv, it) = assignTemp (List.hd newEnvs5) iLabel true
                                    in
                                        (updateFirstVal newEnvs5 iEnv, it)
                                    end
                    | _                    => (newEnvs5, Temp.DUMMY_VALUE)

                val initInst = case startRes of
                                      IntRes  i => [CTM.mLi t i CTM.DUMMY_STR]
                                    | TempRes v => [CTM.mMove t v CTM.DUMMY_STR]

                val condInst =
                        let
                            val (gtc, ltc) = case endRes of
                                  IntRes  i => ([CTM.mBgt_I t i loopEnd], [CTM.mBle_I t i loopEnd])
                                | TempRes v => ([CTM.mBgt   t v loopEnd], [CTM.mBle   t v loopEnd])

                            val terminatingInsts = [CTM.mLabel gtCondition Temp.DUMMY_VALUE] @ gtc @
                                                    [CTM.mJ loopBody Temp.DUMMY_VALUE] @
                                                    [CTM.mLabel ltCondition Temp.DUMMY_VALUE] @ ltc

                            val jInsts = case (startRes, endRes) of
                                  (IntRes  i, IntRes  j) => [CTM.mLi nt i CTM.DUMMY_STR,
                                                            CTM.mBle_I  nt j gtCondition,
                                                            CTM.mBgt_I  nt j ltCondition]
                                | (TempRes i, IntRes  j) => [CTM.mBle_I i  j gtCondition,
                                                            CTM.mBgt_I  i  j ltCondition]
                                | (IntRes  i, TempRes j) => [CTM.mBge_I j  i gtCondition,
                                                            CTM.mBlt_I  j  i ltCondition]
                                | (TempRes i, TempRes j) => [CTM.mBle   i  j gtCondition,
                                                            CTM.mBgt    i  j ltCondition]
                        in
                            jInsts @ terminatingInsts
                        end

                val stepInst = case stepRes of
                                      IntRes  i => [CTM.mAddi t t i CTM.DUMMY_STR]
                                    | TempRes v => [CTM.mAdd  t t v CTM.DUMMY_STR]

                val jumpInst = [CTM.mJ loopCondition Temp.DUMMY_VALUE]

                val loopInsts = [CTM.mLabel loopStart Temp.DUMMY_VALUE] @
                                initInst @
                                [CTM.mLabel loopCondition Temp.DUMMY_VALUE] @
                                condInst @
                                [CTM.mLabel loopBody Temp.DUMMY_VALUE] @
                                bodyInsts @
                                [CTM.mLabel loopStep Temp.DUMMY_VALUE] @
                                stepInst @ jumpInst @
                                [CTM.mLabel loopEnd Temp.DUMMY_VALUE]
            in
                (resEnvs, startInsts @ endInsts @ stepInsts @ loopInsts,
                                        startData @ endData @ stepData @ bodyData)
            end

    (* Utility function manager for translating Tiger.Print expressions *)
    (* printExprHelper : Env.mp list -> Tiger.Expr
                                            -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and printExprHelper envs expr =
            let
                val (aEnv, a0) = assignTemp (getLastVal envs) Utils.A0_REG false  (* For register a0 *)
                val _          = RA.allocRegWith a0 Mips.A0

                val (newEnv, t) = assignTemp aEnv Utils.V0_REG false  (* For register v0 *)
                val _           = RA.allocRegWith t Mips.V0

                val newEnvs = updateLastVal envs newEnv

                (* Instruction set for printing *)
                val printInsts = CTM.mSyscall t Utils.PRINT_INT_SYSCALL CTM.DUMMY_STR
            in
                (case expr of
                      TIG.Int  i  => (newEnvs, [CTM.mLi a0 i CTM.DUMMY_STR] @ printInsts, [])
                    | TIG.Lval l  => (case l of
                                        TIG.Var id => (newEnvs,
                                                        [CTM.mMove a0 (getTemp envs id) CTM.DUMMY_STR]
                                                            @ printInsts, []
                                                        )
                                        )
                    | TIG.Op r    =>
                        let
                            val {left, oper, right} = r
                            val (lRes, newEnvs1, lInsts, lData) = evalExpr newEnvs left
                            val (rRes, newEnvs2, rInsts, rData) = evalExpr newEnvs right
                            val (addInsts, addData) = evalReducedOpExpr a0 lRes oper rRes
                            val resEnv              = Env.union (List.hd newEnvs1) (List.hd newEnvs2)
                        in
                            (updateFirstVal newEnvs resEnv,
                                    lInsts @ rInsts @ addInsts @ printInsts, lData @ rData @ addData)
                        end
                    | TIG.Neg e   =>
                        let
                            val (res, resEnvs, insts, data) = evalExpr newEnvs e
                            val (addInsts, addData)         = evalReducedNegExpr a0 res
                        in
                            (resEnvs, insts @ addInsts @ printInsts, data @ addData)
                        end
                    | TIG.Exprs e =>
                        let
                            val (res, resEnvs, insts, data) = evalExprs newEnvs e
                            val (addInsts, addData) = assignExprSimplified a0 res
                        in
                            (resEnvs, insts @ addInsts @ printInsts, data @ addData)
                        end
                    | e           => (raiseUnsupportedOperationException
                                        ("[printExprHelper]: Operation not supported:\n" ^
                                            (PTA.prettyTig (TIG.Expression e)))
                                        )
                )
            end

    (* Compiles Tiger program to Ir program *)
    (* compileToIR : Tiger.Prog -> Env.mp list * Ir.Prog *)
    and compileToIR (TIG.Expression e) =
            let
                val (newEnvs1, insts, data) = translateExpr [Env.empty()] e

                val headerDirs  = [Mips.Data] @ data @ [Mips.Text, Mips.Globl "main"]
                val headerStmts = (map (fn d => CTM.mapDirToStmt d CTM.DUMMY_STR Temp.DUMMY_VALUE)
                                            headerDirs
                                    )

                (* For register v0 *)
                val (newEnv, t) = assignTemp (getLastVal newEnvs1) Utils.V0_REG false
                val _           = RA.allocRegWith t Mips.V0

                val newEnvs2 = updateLastVal newEnvs1 newEnv

                val exitInsts = CTM.mSyscall (getTemp newEnvs2 Utils.V0_REG) Utils.EXIT_SYSCALL CTM.DUMMY_STR
            in
                (newEnvs2, headerStmts @ [CTM.mLabel "main" Temp.DUMMY_VALUE] @ insts @ exitInsts)
            end

    (* Compiles Ir program to MIPS *)
    (* compileToMips : Ir.Prog -> (string, Mips.Reg) Mips.Prog *)
    and compileToMips prog = Mips.mapProg Utils.identity RA.getReg prog
end
