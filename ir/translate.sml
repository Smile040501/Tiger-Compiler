(* Structure to translate Tiger.Prog to Ir.Prog and then to Mips.Prog *)
signature TRANSLATE =
sig
    val temps         : (string * Temp.value) list ref
    val compileToIR   : Tiger.Prog -> Env.mp list * Ir.Prog
    val compileToMips : Ir.Prog -> (string, Mips.Reg) Mips.Prog
end

structure Translate :> TRANSLATE =
struct

    (* structures used *)
    structure TIG = Tiger
    structure PTA = PrettyTigerAST
    structure CTM = ConvToMIPS
    structure RA  = RegAlloc

    (* Utility functions *)
    val updateFirstVal = Utils.updateFirstVal
    val updateLastVal  = Utils.updateLastVal
    val getLastVal     = Utils.getLastVal

    (* The result type of intermediate (nested) expressions
       Either it will be an integer result or a resultant register  *)
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

    (* The temporaries allocation performed by the compiler.
       To keep track of all the temporaries (including the nested ones). *)
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
                                (* Storing the (id, t) pair in the list created *)
                                val _      = temps := (!temps @ [(id, t)])
                                (* Allocating register to it if mentioned *)
                                val _      = if allocateReg then (RA.allocReg t)
                                             else ()
                            in
                                (newEnv, t)
                            end
            )

    (* Get the temporary value allocated to the Tiger.Lvalue
       Searches for the variable from the current scope to outer scopes. *)
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
                (* Recursively evaluate the left and the right expressions *)
                val (rRes, newEnvs2, rInsts, rData) = evalExpr envs right
                (*  Take the union of both the updated envs *)
                val newEnv3      = Env.union (List.hd newEnvs1) (List.hd newEnvs2)
                (* As it being nested expression, make a new label(variable)
                   for allocating temporary to it. *)
                val tempLabel    = Temp.newLabel ()
                (* Assign temp to this new label(variable) *)
                val (newEnv4, t) = assignTemp newEnv3 tempLabel true
                (* Solve the simplified binary expression *)
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
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR,
                                                    CTM.mAddi t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mAddi t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mAddi t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mAdd  t i j CTM.DUMMY_STR], [])
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Minus rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR,
                                                    CTM.mSub_I t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mSub_I t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mSub_I t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mSub   t i j CTM.DUMMY_STR], [])
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Mul rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR,
                                                    CTM.mMul_I t t j CTM.DUMMY_STR], [])
                | (IntRes  i, TempRes j) => ([CTM.mMul_I t j i CTM.DUMMY_STR], [])
                | (TempRes i, IntRes  j) => ([CTM.mMul_I t i j CTM.DUMMY_STR], [])
                | (TempRes i, TempRes j) => ([CTM.mMul   t i j CTM.DUMMY_STR], [])
            )
      | evalReducedOpExpr (t: Temp.value) lRes TIG.Div rRes =
            (case (lRes, rRes) of
                  (IntRes  i, IntRes  j) => ([CTM.mLi t i CTM.DUMMY_STR,
                                                    CTM.mDiv_QI t t j CTM.DUMMY_STR], [])
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
                (* Recursively evaluate the expression *)
                (* As it being nested expression, make a new label(variable)
                   for allocating temporary to it. *)
                val tempLabel   = Temp.newLabel ()
                (* Assign temp to this new label(variable) *)
                val (newEnv, t) = assignTemp (List.hd newEnvs) tempLabel true
                (* Solve the simplified negation expression *)
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
                | [e]       => evalExpr envs e  (* Evaluate the nested expression *)
                | (e :: es) => let
                                   val (res, newEnvs1, insts1, data1)  = evalExpr envs e
                                   (* Recursively evaluate the expressions *)
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
                  (TIG.Assign  e) => assignExprHelper envs (#lvalue e) (#expr e)
                | (TIG.For     e) => (forExprHelper envs
                                        (#loopVar e) (#startPos e) (#endPos e) (#step e) (#body e)
                                    )
                | (TIG.Print   e) => printExprHelper envs e
                | (TIG.Println e) => printlnExprHelper envs e
                | (TIG.Exprs   e) => translateExprs envs e
                | _               => (envs, [], [])
            )

    (* Translates the Tiger.Expr list to Ir.Stmt list *)
    (* tranlateExprs : Env.mp list -> Tiger.Expr list
                                        -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and translateExprs (envs: Env.mp list) (exprs: TIG.Expr list) =
            (case exprs of
                  []        => (envs, [], [])
                | (e :: es) => let
                                   val (newEnvs1, insts1, data1) = translateExpr envs e
                                   (* Recursively translate the expressions *)
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
                val (newEnv, t) = (case lval of (* Assigning temporary to LHS *)
                                        TIG.Var v => assignTemp (List.hd envs) v true
                                    )
                (* Need to update this newEnv with the first value of the envs *)
            in
                (case expr of
                        (* Simply loads the immediate value *)
                      TIG.Int i  => (updateFirstVal envs newEnv, [CTM.mLi t i CTM.DUMMY_STR], [])

                    (* Simply a move instruction from one register to another *)
                    | TIG.Lval l => (case l of
                                        TIG.Var id => (updateFirstVal envs newEnv,
                                                        [CTM.mMove t (getTemp envs id) CTM.DUMMY_STR], []
                                                       )
                                    )

                    (* First recursively evaluate the left and right expression to get a resultant type expression for both and then simply using the binary op instruction in MIPS *)
                    | TIG.Op r   =>
                        let
                            val {left, oper, right} = r
                            val (lRes, newEnvs1, lInsts, lData) = evalExpr envs left
                            val (rRes, newEnvs2, rInsts, rData) = evalExpr envs right
                            (* The simplified instructions for binary operation *)
                            (* Passing `t` also assigns it simultaneously *)
                            val (addInsts, addData) = evalReducedOpExpr t lRes oper rRes
                            (* Take union of envs from both sides and with the `newEnv` above *)
                            val newEnv3             = Env.union (List.hd newEnvs1) (List.hd newEnvs2)
                            val resEnv              = Env.union newEnv3 newEnv
                        in
                            (updateFirstVal envs resEnv,
                                    lInsts @ rInsts @ addInsts, lData @ rData @ addData)
                        end

                    (* First recursively evaluate the expression to get a resultant type expression and then simply using the neg op instruction in MIPS *)
                    | TIG.Neg e  =>
                        let
                            val (res, newEnvs, insts, data) = evalExpr envs e
                            (* The simplified instructions for negation operation *)
                            (* Passing `t` also assigns it simultaneously *)
                            val (addInsts, addData) = evalReducedNegExpr t res
                            (* Take union of the env with the `newEnv` above *)
                            val resEnv              = Env.union (List.hd newEnvs) newEnv
                        in
                            (updateFirstVal envs resEnv, insts @ addInsts, data @ addData)
                        end

                    (* First recursively evaluate all the expressions to get a resultant type expression. The result will be the resultant type of the last expression in the list. Then simply do the assignment for x := res *)
                    | TIG.Exprs e =>
                        let
                            val (res, newEnvs, insts, data) = evalExprs envs e
                            (* The simplified instruction for the assignment *)
                            val (addInsts, addData) = assignExprSimplified t res
                            (* Take union of the env with the `newEnv` above *)
                            val resEnv              = Env.union (List.hd newEnvs) newEnv
                        in
                            (updateFirstVal envs resEnv, insts @ addInsts, data @ addData)
                        end

                    (* Other than the above cases, all other assignment operations are not allowed. Currently, the compiler doesn't support `unit` type for allowing such operations *)
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
                (* Generating labels for different jump positions in MIPS assembly *)
                (*
                    Design for the general for-loop in MIPS assembly:
                        for x := i to j by k do body done;

                    forStart:   Li x, i
                    forCond:    Ble i, j, GTC       (* Increasing for loop: i <= j *)
                                Bgt i, j, LTC       (* Decreasing for loop: i > j *)
                    GTC:        Bgt x, j, forEnd    (* End if x > j *)
                                J forBody
                    LTC:        Blt x, j, forEnd    (* End if x < j *)
                    forBody:    [body]
                    forStep:    add x, x, k   (* For supporting `continue`, we need to jump here *)
                                J forCond
                    forEnd:                   (* For supporting `break`, we need to jump here *)
                *)
                val loopStart     = "forStart" ^ forLoop    (* Start of the loop          *)
                val loopCondition = "forCond"  ^ forLoop    (* Condition for the loop     *)
                val gtCondition   = "forGT"    ^ forLoop    (* To be used if start <= end *)
                val ltCondition   = "forLT"    ^ forLoop    (* To be used if start > end  *)
                val loopBody      = "forBody"  ^ forLoop    (* Body of the loop           *)
                val loopStep      = "forStep"  ^ forLoop    (* Step of the loop           *)
                val loopEnd       = "forEnd"   ^ forLoop    (* End of the loop            *)

                (* Assign new temporary for the loop variable in a new empty env *)
                val (newEnv, t) = assignTemp (Env.empty ()) loopVar true

                (* Solve the `start`, `end` and `step` expressions without changing the env *)
                val (startRes, startEnvs, startInsts, startData) = evalExpr envs startPos
                val (endRes,   endEnvs,   endInsts,   endData  ) = evalExpr envs endPos
                val (stepRes,  stepEnvs,  stepInsts,  stepData ) = evalExpr envs step

                (* Solve the `body` expression with new and old envs *)
                val (bodyEnvs, bodyInsts, bodyData) = translateExpr (newEnv :: envs) body

                (* Update the envs first value using result env of `start`, `end` and `step` expr *)
                val newEnv2 = Env.union (List.hd startEnvs) (List.hd endEnvs)
                val newEnv3 = Env.union newEnv2 (List.hd stepEnvs)
                val newEnvs4 = updateFirstVal envs newEnv3

                (* Only thing matter from result env of body is that last env, that could be modified in case of print instruction. So, need to update that too. *)
                val newEnvs5 = updateLastVal newEnvs4 (getLastVal bodyEnvs)

                (* In case if both the `start` and `end` expressions are integers, we need to first move one integer to a register in order to compare them in `forCond` *)
                val (resEnvs, nt) = case (startRes, endRes) of
                      (IntRes _, IntRes _) =>
                                    let
                                        val iLabel = Temp.newLabel() (* new label for temp register *)
                                        (* Assign temporary to this label *)
                                        val (iEnv, it) = assignTemp (List.hd newEnvs5) iLabel true
                                    in
                                        (* Return updated env and the temp assigned to this label *)
                                        (updateFirstVal newEnvs5 iEnv, it)
                                    end
                    | _                    => (newEnvs5, Temp.DUMMY_VALUE) (* No change in the env *)

                (* The initial instructions of the `forStart` *)
                val initInst = case startRes of
                                      IntRes  i => [CTM.mLi   t i CTM.DUMMY_STR]
                                    | TempRes v => [CTM.mMove t v CTM.DUMMY_STR]

                (* The instructions for the `forCond`, `GTC` and `LTC` *)
                val condInst =
                        let
                            val (gtc, ltc) = case endRes of (* For `GTC` and `LTC` *)
                                  IntRes  i => ([CTM.mBgt_I t i loopEnd], [CTM.mBlt_I t i loopEnd])
                                | TempRes v => ([CTM.mBgt   t v loopEnd], [CTM.mBlt   t v loopEnd])

                            (* Overall instructions with labels for `GTC` and `LTC` *)
                            val terminatingInsts = [CTM.mLabel gtCondition Temp.DUMMY_VALUE] @ gtc @
                                                    [CTM.mJ loopBody Temp.DUMMY_VALUE] @
                                                    [CTM.mLabel ltCondition Temp.DUMMY_VALUE] @ ltc

                            (* Jump conditions instructions for the `forCond` *)
                            val jInsts = case (startRes, endRes) of
                                    (* Move a value `i` to the temporary created for it *)
                                  (IntRes  i, IntRes  j) => [CTM.mLi    nt i CTM.DUMMY_STR,
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

                (* The instructions for the `forStep` *)
                val stepInst = case stepRes of
                                      IntRes  i => [CTM.mAddi t t i CTM.DUMMY_STR]
                                    | TempRes v => [CTM.mAdd  t t v CTM.DUMMY_STR]

                (* Jump instruction of the for loop, part of `forStep` *)
                val jumpInst = [CTM.mJ loopCondition Temp.DUMMY_VALUE]

                (* The final loop instructions with labels *)
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
                (* Assigning temporaries for special registers in the last (global) env. As they should be only assigned temporary once and only assigned registers once. *)
                (* Allocating them special registers on our own *)
                val (newEnv, t) = assignTemp aEnv Utils.V0_REG false  (* For register v0 *)
                val _           = RA.allocRegWith t Mips.V0

                (* Updating the envs' last value (global environment) *)
                val newEnvs = updateLastVal envs newEnv

                (* Instruction set for printing *)
                (* Loading the `syscall` number to `v0` and calling the `syscall` instruction *)
                val printInsts = CTM.mSyscall t Utils.PRINT_INT_SYSCALL CTM.DUMMY_STR
            in
                (case expr of
                        (* Move the value to the register a0 as argument to `syscall` *)
                      TIG.Int  i  => (newEnvs, [CTM.mLi a0 i CTM.DUMMY_STR] @ printInsts, [])
                    | TIG.Lval l  => (case l of
                                        TIG.Var id => (newEnvs,
                                                        [CTM.mMove a0 (getTemp envs id) CTM.DUMMY_STR]
                                                            @ printInsts, []
                                                        )
                                        )

                    (* Recursively evaluate left and right expressions and put the result of binary operation to the register a0 *)
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

                    (* Recursively evaulate the expression and put the result of negation operation to the register a0 *)
                    | TIG.Neg e   =>
                        let
                            val (res, resEnvs, insts, data) = evalExpr newEnvs e
                            val (addInsts, addData)         = evalReducedNegExpr a0 res
                        in
                            (resEnvs, insts @ addInsts @ printInsts, data @ addData)
                        end

                    (* Recursively evaluate the expressions and move the result of the last expression to the register a0 *)
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

    (* Utility function manager for translating Tiger.Println expressions *)
    (* printlnExprHelper : Env.mp list -> Tiger.Expr
                                            -> Env.mp list * Ir.Stmt list * Ir.Stmt list *)
    and printlnExprHelper envs expr =
            let
                val (newEnvs, insts, data) = printExprHelper envs expr
                (* Instructions for simple printing of the expression *)
                (* The `nl` label is always put in the data section of the MIPS by default *)

                (* Instruction set for calling the `print_string` syscall *)
                val printInsts = CTM.mSyscall (getTemp newEnvs Utils.V0_REG)
                                            Utils.PRINT_STRING_SYSCALL CTM.DUMMY_STR
            in
                (newEnvs,
                    (* Loading the address of the `nl` label to the a0 register *)
                    insts @ [CTM.mLa (getTemp newEnvs Utils.A0_REG) Utils.NL_Label] @ printInsts,
                data)
            end

    (* Compiles Tiger program to Ir program *)
    (* compileToIR : Tiger.Prog -> Env.mp list * Ir.Prog *)
    and compileToIR (TIG.Expression e) =
            let
                val (newEnvs1, insts, data) = translateExpr [Env.empty()] e
                (* Instructions statements for translated expressions and updated envs *)

                (* Default header for the MIPS assembly
                        .data
                    nl: .asciiz "\n"
                        .text
                        .globl main
                    main:
                *)
                val headerStmts  = [CTM.mDir Mips.Data CTM.DUMMY_STR Temp.DUMMY_VALUE] @ data @
                            [CTM.mLabel Utils.NL_Label Temp.DUMMY_VALUE] @
                            [CTM.mDir (Mips.Asciiz "\\n") CTM.DUMMY_STR Temp.DUMMY_VALUE] @
                            [CTM.mDir Mips.Text CTM.DUMMY_STR Temp.DUMMY_VALUE] @
                            [CTM.mDir (Mips.Globl "main") CTM.DUMMY_STR Temp.DUMMY_VALUE] @
                            [CTM.mLabel "main" Temp.DUMMY_VALUE]

                (* For register v0. Either it will be assigned by instructions or assigning now. *)
                val (newEnv, t) = assignTemp (getLastVal newEnvs1) Utils.V0_REG false
                val _           = RA.allocRegWith t Mips.V0

                (* We need to update the global environment i.e. last value in the env list *)
                val newEnvs2 = updateLastVal newEnvs1 newEnv

                (* Instructions for calling the `exit` syscall *)
                val exitInsts = CTM.mSyscall (getTemp newEnvs2 Utils.V0_REG) Utils.EXIT_SYSCALL CTM.DUMMY_STR
            in
                (newEnvs2, headerStmts @ insts @ exitInsts)
            end

    (* Compiles Ir program to MIPS *)
    (* compileToMips : Ir.Prog -> (string, Mips.Reg) Mips.Prog *)
    and compileToMips prog = Mips.mapProg Utils.identity RA.getReg prog
end
