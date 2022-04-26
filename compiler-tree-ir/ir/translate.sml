(* Structure to translate Tiger.Prog to Tree.Stm *)
signature TRANSLATE =
sig
    (* Kinds of expressions in the Tree language *)
    datatype Exp = Ex of Tree.Exp       (* An Expression *)
                |  Nx of Tree.Stm       (* No result *)
                (* Conditional: Pass it a true-destination and a false-destination. It will make a
                statement that evaluates some conditionals and then jump to one of the destinations *)
                |  Cx of Temp.label * Temp.label -> Tree.Stm

    val unEx : Exp -> Tree.Exp
    val unNx : Exp -> Tree.Stm
    val unCx : Exp -> (Temp.label * Temp.label -> Tree.Stm)

    val compileToIR : Tiger.Prog -> Tree.Stm
end

structure Translate :> TRANSLATE =
struct

    (* structures used *)
    structure TIG = Tiger
    structure T   = Tree
    structure PTA = PrettyTigerAST


    (* Raising exception and printing the error message *)
    exception ConditionalNoReturn   of string
    exception NoReturnToConditional of string
    exception UndefinedVariable     of string
    exception RestrictionFailed     of string
    exception ImplementationError   of string
    exception Unimplemented         of string
    exception UnsupportedOperation  of string

    fun raiseException (ex: string -> exn) (msg: string) =
                    Utils.throwErr ex ("[translate.sml]:" ^ msg ^ "\n\n")


    (* Kinds of expressions in the Tree language *)
    datatype Exp = Ex of Tree.Exp  (* An Expression *)
                |  Nx of Tree.Stm  (* No result *)
                (* Conditional: Pass it a true-destination and a false-destination. It will make a
                statement that evaluates some conditionals and then jump to one of the destinations *)
                |  Cx of Temp.label * Temp.label -> Tree.Stm


    (* Returns the Tree.Stm from the Tree.Exp *)
    (* val getStmt : Tree.Exp -> Tree.Stm *)
    fun getStmt (T.ESEQ {stm, res}) = stm
      | getStmt _                   = raiseException RestrictionFailed "[getStmt]: Not an ESEQ"

    (* Strips off the constructors (Ex, Nx, Cx) and returns Tree.Exp *)
    (* val unEx : Exp -> Tree.Exp *)
    fun unEx (Ex e)   = e
      | unEx (Nx s)   =
            let
                val stmtAfterMoving = T.seq [
                    s,
                    T.MOVE (T.getMoveRec T.resultTemp (T.CONST 0))
                ]
            in
                T.ESEQ (T.getEseqRec stmtAfterMoving T.resultTemp)
            end
      | unEx (Cx con) =
            let
                val t    = Temp.newLabel()  (* True destination  *)
                val f    = Temp.newLabel()  (* False destination *)
                val cont = Temp.newLabel()  (* Continuation      *)
            in
                T.ESEQ (T.getEseqRec
                    (T.seq [
                        (* Conditional jump *)
                        con (t, f),
                        (* If condition succeeds, it enters this block *)
                        T.LABEL t,
                        (* Move 1 into the register *)
                        T.MOVE (T.getMoveRec T.resultTemp (T.CONST 1)),
                        (* Jump to the continuation *)
                        T.JUMP (T.getJumpRec (T.NAME cont) [cont]),
                        (* If condition fails, it enters this block *)
                        T.LABEL f,
                        (* Set register to 0 as condition failed *)
                        T.MOVE (T.getMoveRec T.resultTemp (T.CONST 0)),
                        (* Continue *)
                        T.LABEL cont
                    ])
                    (T.resultTemp)  (* Result is just contains 0 or 1 *)
                )
            end

    (* Strips off the constructors (Ex, Nx, Cx) and returns Tree.Stm *)
    (* val unNx : Exp -> Tree.Stm *)
    fun unNx (Ex e)      = getStmt e
      | unNx (Nx s)      = s
      | unNx (e as Cx _) = raiseException ConditionalNoReturn "[unNx]: Conditional expression has no return"

    (* Strips off the constructors (Ex, Nx, Cx) and generates a conditional statement function *)
    (* val unCx : Exp -> (Temp.label * Temp.label -> Tree.Stm) *)
    fun unCx (Ex e)   = (fn (t, f) => T.seq [
              getStmt e,
              T.MOVE (T.getMoveRec T.argTemp1 T.resultTemp),
              T.MOVE (T.getMoveRec T.argTemp2 (T.CONST 0)),
              (* If value in resultTemp is 1 then jump to true label else to false label *)
              T.CJUMP (T.getCjumpRec T.argTemp1 T.NE T.argTemp2 t f)
          ])
      | unCx (Nx _)   = raiseException NoReturnToConditional "[unCx]: No return to conditional"
      | unCx (Cx con) = con


    (* Simplifies the nested expression into Exp *)
    (* val evalExpr : Info.Info list -> Tiger.Expr ->
                                                  (Info.Info list * Exp) *)
    fun evalExpr (infos: Info.Info list) (exp: TIG.Expr) : (Info.Info list * Exp) =
          (case exp of
                  (TIG.Int   i) => evalIntExpr          infos i
                | (TIG.Lval  l) => evalLvalueExpr       infos l
                | (TIG.Op    r) => evalOpExpr           infos (#left r) (#oper r) (#right r)
                | (TIG.Neg   e) => evalNegExpr          infos e
                | (TIG.Exprs e) => translateOrEvalExprs infos e true
                |  e            => (raiseException UnsupportedOperation
                                        ("[evalExpr]: Nested operation not supported!\n" ^
                                            (PTA.prettyTig (TIG.Expression e)))
                                    )
            )

    (* Simplifies the nested integer expression into Exp *)
    (* val evalIntExpr : Info.Info list -> int ->
                                                (Info.Info list * Exp) *)
    and evalIntExpr (infos: Info.Info list) (i: int) : (Info.Info list * Exp) =
          (infos, Ex (T.ESEQ (
                (* resultTemp = i *)
                T.getEseqRec (T.MOVE (T.getMoveRec T.resultTemp (T.CONST i))) T.resultTemp
            )))

    (* Simplifies the nested lvalue expression into Exp *)
    (* val evalLvalueExpr : Info.Info list -> TIG.Lvalue ->
                                                        (Info.Info list * Exp) *)
    and evalLvalueExpr (infos: Info.Info list) (l: TIG.Lvalue) : (Info.Info list * Exp) =
          (case l of
              (TIG.Var var) =>
                  let
                      val frames = map Info.getCurrFrame infos    (* Current Frame *)
                      (* Get offset of the variable in the frame *)
                      val optNameVarOffset = Frame.getVarOffset frames var

                      val (funContVar, varOffset) = case optNameVarOffset of
                            (* Variable is not defined *)
                              NONE                  => raiseException UndefinedVariable
                                                            ("[evalLvalueExpr]: \"" ^ var
                                                                          ^ "\" is not defined")
                            (* Variable is defined *)
                            | SOME fname_var_offset => fname_var_offset

                      (* Get the frame pointer statement *)
                      val framePointerStmt =
                                  Frame.getFramePointer (Info.getCurrFunc (List.hd infos)) funContVar

                      (* Sequence of statements to be executed *)
                      val computeAndMoveToTemp = T.seq [
                          (* resultTemp = framePointer *)
                          framePointerStmt,
                          (* resultTemp = *(resultTemp + varOffset) *)
                          T.MOVE (T.getMoveRec T.resultTemp (T.MEM (
                              T.BINOP (
                                  T.getBinopRec T.resultTemp T.PLUS (T.CONST varOffset)
                              )
                          )))
                      ]
                  in
                      (infos, Ex (T.ESEQ (T.getEseqRec computeAndMoveToTemp T.resultTemp)))
                  end
          )

    (* Simplifies the nested binary operator expression into Exp *)
    (* val evalIntExpr : Info.Info list -> Tiger.Expr -> Tiger.BinOp -> Tiger.Expr ->
                                                                              (Info.Info list * Exp) *)
    and evalOpExpr (infos: Info.Info list) (left: TIG.Expr) (oper: TIG.BinOp) (right: TIG.Expr)
                                                                    : (Info.Info list * Exp) =
          let
              val (infos1, evalEx1) = evalExpr infos left
              (* Recursively evaluate the left expression *)
              val _ = Utils.assertNonEmptyList infos1
              val (i1, is1) = Utils.separateFirstVal infos1

              (* Allocate space for the result of left expression on the stack *)
              (* exOffset1 is where we need to move the result of the left expression *)
              val (frame1, exOffset1) = Frame.allocInternalVar (Info.getCurrFrame i1)
              val ex1 = unEx evalEx1  (* Get the Tree.Exp from the Exp *)

              (* Recursively evaluate the right expression *)
              val (infos2, evalEx2) =
                                      evalExpr ((Info.setCurrFrame i1 frame1) :: is1) right
              val _ = Utils.assertNonEmptyList infos2
              val (i2, is2) = Utils.separateFirstVal infos2

              (* Allocate space for the result of right expression on the stack *)
              (* exOffset2 is where we need to move the result of the right expression *)
              val (frame2, exOffset2) = Frame.allocInternalVar (Info.getCurrFrame i2)
              val ex2 = unEx evalEx2  (* Get the Tree.Exp from the Exp *)

              (* Update the frame and info about the number of variables allocated *)
              val info3 = Info.setCurrFrame i2 frame2
              val info4 = Info.updateNumAllocs info3 2

              (* We will store the result of left and right to T.argTemp1 and T.argTemp2 *)
              val ex = case oper of
                    TIG.Plus  => T.BINOP (T.getBinopRec T.argTemp1 T.PLUS  T.argTemp2)
                  | TIG.Minus => T.BINOP (T.getBinopRec T.argTemp1 T.MINUS T.argTemp2)
                  | TIG.Mul   => T.BINOP (T.getBinopRec T.argTemp1 T.MUL   T.argTemp2)
                  | TIG.Div   => T.BINOP (T.getBinopRec T.argTemp1 T.DIV   T.argTemp2)

              (* The result of the expression is stored in resultTemp *)
              val stmt = case ex of
                      T.ESEQ _ => getStmt ex
                    | _        => T.MOVE (T.getMoveRec T.resultTemp ex)

              (* Sequence of statements to be executed *)
              val computeAndMoveToTemp = T.seq [
                  (* resultTemp = eval(left) *)
                  getStmt ex1,
                  (* *exOffset1 = resultTemp *)
                  T.moveTempToFrame exOffset1 T.resultTemp,
                  (* resultTemp = eval(right) *)
                  getStmt ex2,
                  (* *exOffset2 = resultTemp = eval(right) *)
                  T.moveTempToFrame exOffset2 T.resultTemp,
                  (* argTemp1 = *exOffset1 *)
                  T.moveFrameToTemp T.argTemp1 exOffset1,
                  (* argTemp2 = *exOffset2 *)
                  T.moveFrameToTemp T.argTemp2 exOffset2,
                  (* resultTemp = argTemp1 + argTemp2 *)
                  stmt
              ]
          in
              (info4 :: is2, Ex (T.ESEQ (T.getEseqRec computeAndMoveToTemp T.resultTemp)))
          end

    (* Simplifies the nested negation expression into Exp *)
    (* val evalNegExpr : Info.Info list -> Tiger.Expr ->
                                                (Info.Info list * Exp) *)
    and evalNegExpr (infos: Info.Info list) (exp: Tiger.Expr) : (Info.Info list * Exp) =
          let
              val (newInfos, transEx) = evalExpr infos exp
              (* Recursively evaluate the expression *)
              val ex = unEx transEx   (* Get the Tree.Exp from the Exp *)

              (* Sequence of statements to be executed *)
              val computeAndMoveToTemp = T.seq [
                  (* resultTemp = eval(exp) *)
                  getStmt ex,
                  (* argTemp1 = 0 *)
                  T.MOVE (T.getMoveRec T.argTemp1 (T.CONST 0)),
                  (* resultTemp = argTemp1 - resultTemp *)
                  T.MOVE (
                    T.getMoveRec T.resultTemp (T.BINOP (T.getBinopRec T.argTemp1 T.MINUS T.resultTemp))
                  )
              ]
          in
              (newInfos, Ex (T.ESEQ (T.getEseqRec computeAndMoveToTemp T.resultTemp)))
          end


    (* Translates Tiger.Expr to Exp
        It will be called for the top-level expressions only *)
    (* val translateExpr : Info.Info list -> Tiger.Expr ->
                                                       (Info.Info list * Exp) *)
    and translateExpr (infos: Info.Info list) (exp: TIG.Expr) : (Info.Info list * Exp) =
          (case exp of
                TIG.Nil         => (raiseException Unimplemented
                                        "[translateExpr]: Nil expressions are not implemented!"
                                    )
              | (TIG.Int     i) => evalIntExpr          infos i
              | (TIG.Lval    l) => evalLvalueExpr       infos l
              | (TIG.Op      r) => evalOpExpr           infos (#left r) (#oper r) (#right r)
              | (TIG.Neg     e) => evalNegExpr          infos e
              | (TIG.Assign  e) => translateAssignExpr  infos (#lvalue e) (#expr e)
              | (TIG.For     e) => translateForExpr     infos e
              | (TIG.Print   e) => translatePrintExpr   infos e "print"
              | (TIG.Println e) => translatePrintExpr   infos e "println"
              | (TIG.Exprs   e) => translateOrEvalExprs infos e false
          )

    (* Utility function manager for translating Tiger.Assign expressions *)
    (* val translateAssignExpr : Info.Info list -> Tiger.Lvalue -> Tiger.Expr ->
                                                                          (Info.Info list * Exp) *)
    and translateAssignExpr (infos: Info.Info list) (lval: TIG.Lvalue) (expr: TIG.Expr)
                                                                          : (Info.Info list * Exp) =
          (case lval of
            TIG.Var var =>
                let
                    val optNameVarOffset = Frame.getVarOffset [Info.getCurrFrame (List.hd infos)] var
                    (* Get offset of the variable in the current frame only *)
                in
                    case optNameVarOffset of
                          NONE   => translateAssignExprUndefined infos lval expr
                        | SOME _ => translateAssignExprDefined   infos lval expr
                end
          )

    (* Utility function manager for translating Tiger.Assign expressions
        when the LHS is not defined *)
    (* val translateAssignExprUndefined : Info.Info list -> Tiger.Lvalue -> Tiger.Expr ->
                                                                          (Info.Info list * Exp) *)
    and translateAssignExprUndefined (infos: Info.Info list) (lval: TIG.Lvalue) (expr: TIG.Expr)
                                                                          : (Info.Info list * Exp) =
          (case lval of
            TIG.Var var =>
                let
                    val (infos1, transEx) = evalExpr infos expr
                    (* Recursively evaluate the expression *)
                    (* Get the Tree.Exp from the Exp *)
                    val ex = unEx transEx

                    val _ = Utils.assertNonEmptyList infos1
                    val (i1, is1) = Utils.separateFirstVal infos1

                    val currFrame = Info.getCurrFrame i1   (* Get the current frame *)
                    val currFunc  = Info.getCurrFunc  i1   (* Get the current function *)

                    (* Allocate the variable on the stack *)
                    val newFrame = Frame.allocVar currFunc currFrame var

                    (* Get offset of the variable in the current frame only *)
                    val optNameVarOffset = Frame.getVarOffset [newFrame] var
                    val varOffset = case optNameVarOffset of
                        (* Variable is not defined, allocate it *)
                          NONE => (raiseException ImplementationError
                                    "[translateAssignExprUndefined]: Variable was expected to be defined!"
                                  )
                        (* Variable is defined *)
                        | SOME (fname, var_offset) =>
                                    if (fname = currFunc) then
                                        var_offset
                                    else
                                        (raiseException ImplementationError
                                              "[translateAssignExprUndefined]: Function names were expected to be the same!"
                                        )

                    (* Sequence of statements to be executed *)
                    val computeAndMoveToTemp = T.seq [
                        (* resultTemp = eval(exp) *)
                        getStmt ex,
                        (* *varOffset = resultTemp *)
                        T.moveTempToFrame varOffset T.resultTemp
                    ]

                    (* Update the frame and info about the number of variables allocated *)
                    val i2 = Info.setCurrFrame i1 newFrame
                    val i3 = Info.updateNumAllocs i2 1
                in
                    (i3 :: is1, Nx computeAndMoveToTemp)
                end
          )

    (* Utility function manager for translating Tiger.Assign expressions
        when the LHS is already defined *)
    (* val translateAssignExprDefined : Info.Info list -> Tiger.Lvalue -> Tiger.Expr ->
                                                                          (Info.Info list * Exp) *)
    and translateAssignExprDefined (infos: Info.Info list) (lval: TIG.Lvalue) (expr: TIG.Expr)
                                                                          : (Info.Info list * Exp) =
          (case lval of
            TIG.Var var =>
                let
                    val (infos1, transEx) = evalExpr infos expr
                    (* Recursively evaluate the expression *)
                    val ex = unEx transEx     (* Get the Tree.Exp from the Exp *)

                    val _ = Utils.assertNonEmptyList infos1
                    val (i1, is1) = Utils.separateFirstVal infos1

                    (* Reserve space to store the result of RHS *)
                    val (newFrame, storeOffset) = Frame.allocInternalVar (Info.getCurrFrame i1)

                    (* Get offset of the variable in the current frame only *)
                    val optNameVarOffset = Frame.getVarOffset [newFrame] var
                    val (funContVar, varOffset) = case optNameVarOffset of
                        (* Variable is not defined, allocate it *)
                          NONE => (raiseException ImplementationError
                                    "[translateAssignExprDefined]: Variable was expected to be defined!"
                                  )
                        (* Variable is defined *)
                        | SOME fname_var_offset => fname_var_offset

                    (* Get the frame pointer statement *)
                    val framePointerStmt =
                                  Frame.getFramePointer (Info.getCurrFunc (List.hd infos)) funContVar

                    (* Sequence of statements to be executed *)
                    val computeAndMoveToTemp = T.seq [
                        (* resultTemp = eval(exp) *)
                        getStmt ex,
                        (* *storeOffset = resultTemp *)
                        T.moveTempToFrame storeOffset T.resultTemp,
                        (* resultTemp = framePointer *)
                        framePointerStmt,
                        (* argTemp1 = *storeOffset *)
                        T.moveFrameToTemp T.argTemp1 storeOffset,
                        (* *(resultTemp + varOffset) = argTemp1 *)
                        T.MOVE (T.getMoveRec (T.MEM
                                    (T.BINOP (T.getBinopRec T.resultTemp T.PLUS (T.CONST varOffset)))
                                )
                                T.argTemp1
                        )
                    ]

                    (* Update the frame and info about the number of variables allocated *)
                    val i2 = Info.setCurrFrame i1 newFrame
                    val i3 = Info.updateNumAllocs i2 1
                in
                    (i3 :: is1, Nx computeAndMoveToTemp)
                end
          )

    (* Utility function manager for translating Tiger.For expressions *)
    (* val translateForExpr : Info.Info list ->
      {loopVar: Tiger.id, startPos: Tiger.Expr, endPos: Tiger.Expr, step: Tiger.Expr, body: Tiger.Expr}
                                                                      -> (Info.Info list * Exp) *)
    and translateForExpr (infos: Info.Info list) {loopVar, startPos, endPos, step, body}
                                                                      : (Info.Info list * Exp) =
          let
              val loop_label = Temp.newLabel ()   (* Unique label for the loop  *)
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
              (* Different Loop Labels *)
              val loopStart     = "forStart" ^ loop_label    (* Start of the loop          *)
              val loopCondition = "forCond"  ^ loop_label    (* Condition for the loop     *)
              val gtCondition   = "forGT"    ^ loop_label    (* To be used if start <= end *)
              val ltCondition   = "forLT"    ^ loop_label    (* To be used if start > end  *)
              val loopBody      = "forBody"  ^ loop_label    (* Body of the loop           *)
              val loopStep      = "forStep"  ^ loop_label    (* Step of the loop           *)
              val loopEnd       = "forEnd"   ^ loop_label    (* End of the loop            *)

              (* Solve the `start`, `end` and `step` expressions without changing the frame *)
              val (infos1, startEx) = evalExpr infos  startPos
              val (infos2, endEx)   = evalExpr infos1 endPos
              val (infos3, stepEx)  = evalExpr infos2 step
              val startExp = unEx startEx
              val endExp   = unEx endEx
              val stepExp  = unEx stepEx

              val _ = Utils.assertNonEmptyList infos3
              val (i, is) = Utils.separateFirstVal infos3
              (* The current initial frame on entering for loop *)
              val initFrame = Info.getCurrFrame i
              (* Current Function *)
              val currFunc  = Info.getCurrFunc  i

              (* Allocate space for `startExpResult`, `endExprResult` and `stepExprResult` *)
              val (initFrame2, startExpOffset) = Frame.allocInternalVar initFrame
              val (initFrame3, endExpOffset)   = Frame.allocInternalVar initFrame2
              val (initFrame4, stepExpOffset)  = Frame.allocInternalVar initFrame3

              (* Create a new empty frame with same offset as that of `initFrame4` *)
              val newFrame1 = Frame.setOffset Frame.emptyFrame initFrame4

              (* Allocate loopVar onto the new Frame *)
              val newFrame2 = Frame.allocVar currFunc newFrame1 loopVar

              (* Get offset of the loopVar in the current frame only *)
              val optNameVarOffset = Frame.getVarOffset [newFrame2] loopVar
              val varOffset = case optNameVarOffset of
                  (* Variable is not defined, allocate it *)
                    NONE => (raiseException ImplementationError
                              "[translateForExpr]: Loop variable was expected to be defined!"
                            )
                  (* Variable is defined *)
                  | SOME (fname, var_offset) =>
                              if (fname = currFunc) then
                                  var_offset
                              else
                                  (raiseException ImplementationError
                                    "[translateForExpr]: Function names were expected to be the same!"
                                  )

              (* Create new info for the nested scope
                  It has all the info same as previous(head) info except that
                  NewFrame = Frame(prevOffset, map_with_loop_var_only)
                  UpdatedLoopLabel = SOME(loop_label)
                  NewInfo = Info(UpdatedLoopLabel, NewFrame, prevCurrFunc, prevNumAllocs) *)
              val newInfo1 = Info.setCurrFrame i newFrame2
              val newInfo2 = Info.setLoopLabel newInfo1 (SOME loop_label)

              (* Solve the `body` expression with new and old frames *)
              val (infos4, bodyEx) = translateExpr (newInfo2 :: infos3) body
              val bodyExp = unEx bodyEx

              val _ = Utils.assertNonEmptyList infos4
              val (i2, is2) = Utils.separateFirstVal infos4

              (* Update the frame and info about the number of variables allocated
                  UpdatedFrame = Frame(updatedOffset, prevMapAtEntry)
                  Info =  Info(loopLabel, UpddatedFrame, prevCurrFunc, updatedNumAllocs) *)
              val finalFrame = Frame.setOffset initFrame4 (Info.getCurrFrame i2)
              val finalInfo  = Info.setCurrFrame i finalFrame
              val finalInfo2 = Info.setNumAllocs finalInfo (Info.getNumAllocs i2)
              val finalInfo3 = Info.updateNumAllocs finalInfo2 4

              (* Sequence of statements to be executed *)
              val computeAndMoveToTemp = T.seq [
                  (* resultTemp = eval(startExp) *)
                  getStmt startExp,
                  (* *startExpOffset = resultTemp *)
                  T.moveTempToFrame startExpOffset T.resultTemp,

                  (* resultTemp = eval(endExp) *)
                  getStmt endExp,
                  (* *endExpOffset = resultTemp *)
                  T.moveTempToFrame endExpOffset T.resultTemp,

                  (* resultTemp = eval(stepExp) *)
                  getStmt stepExp,
                  (* *stepExpOffset = resultTemp *)
                  T.moveTempToFrame stepExpOffset T.resultTemp,

                  (* forStart *)
                  T.LABEL loopStart,
                  (* argTemp1 = *startExpOffset *)
                  T.moveFrameToTemp T.argTemp1 startExpOffset,
                  (* loopVar = *varOffset = argTemp1 *)
                  T.moveTempToFrame varOffset T.argTemp1,

                  (* forCond *)
                  T.LABEL loopCondition,
                  (* argTemp1 = *startExpOffset *)
                  T.moveFrameToTemp T.argTemp1 startExpOffset,
                  (* argTemp2 = *endExpOffset *)
                  T.moveFrameToTemp T.argTemp2 endExpOffset,
                  (* If start <= end i.e. argTemp1 <= argTemp2, jump to `gtCondition`,
                      else to `ltCondition` *)
                  T.CJUMP (T.getCjumpRec T.argTemp1 T.LE T.argTemp2 gtCondition ltCondition),

                  (* GTC *)
                  T.LABEL gtCondition,
                  (* argTemp1 = *varOffset *)
                  T.moveFrameToTemp T.argTemp1 varOffset,
                  (* argTemp2 = *endExpOffset *)
                  T.moveFrameToTemp T.argTemp2 endExpOffset,
                  (* If loopVar > end i.e. argTemp1 > argTemp2, jump to `loopEnd`,
                      else to `loopBody` *)
                  T.CJUMP (T.getCjumpRec T.argTemp1 T.GT T.argTemp2 loopEnd loopBody),

                  (* LTC *)
                  T.LABEL ltCondition,
                  (* argTemp1 = *varOffset = loopVar *)
                  T.moveFrameToTemp T.argTemp1 varOffset,
                  (* argTemp2 = *endExpOffset *)
                  T.moveFrameToTemp T.argTemp2 endExpOffset,
                  (* If loopVar < end i.e. argTemp1 < argTemp2, jump to `loopEnd`,
                      else to `loopBody` *)
                  T.CJUMP (T.getCjumpRec T.argTemp1 T.LT T.argTemp2 loopEnd loopBody),

                  (* forBody *)
                  T.LABEL loopBody,
                  (* resultTemp = eval(bodyExp) *)
                  getStmt bodyExp,

                  (* forStep *)
                  T.LABEL loopStep,
                  (* argTemp1 = *varOffset = loopVar *)
                  T.moveFrameToTemp T.argTemp1 varOffset,
                  (* argTemp2 = *stepExpOffset *)
                  T.moveFrameToTemp T.argTemp2 stepExpOffset,
                  (* argTemp1 = argTemp1 + argTemp2 *)
                  T.MOVE (T.getMoveRec T.argTemp1 (T.BINOP (T.getBinopRec T.argTemp1 T.PLUS T.argTemp2))),
                  (* loopVar = *varOffset = argTEmp1 *)
                  T.moveTempToFrame varOffset T.argTemp1,
                  (* Jump to `forCond` *)
                  T.JUMP (T.getJumpRec (T.NAME loopCondition) [loopCondition]),

                  (* forEnd *)
                  T.LABEL loopEnd
              ]
          in
              (finalInfo3 :: is, Nx computeAndMoveToTemp)
          end

    (* Utility function manager for translating Tiger.Print and Tiger.Println expressions *)
    (* val translatePrintExpr : Info.Info list -> Tiger.Expr -> string ->
                                                                      (Info.Info list * Exp) *)
    and translatePrintExpr (infos: Info.Info list) (e: TIG.Expr) (funName: string) =
          let
              val (newInfos, transEx) = evalExpr infos e
              (* Recursively evaulate the expression *)
              val ex = unEx transEx

              (* Sequence of statements to be executed *)
              val computeAndMoveToTemp = Tree.seq [
                  getStmt ex,
                  T.EXP (T.CALL (T.getCallRec (T.NAME (Temp.strToLabel funName)) [T.resultTemp]))
              ]
          in
              (newInfos, Nx computeAndMoveToTemp)
          end

    (* Translates Tiger.Expr list to Exp if `nested` is false,
        else simplifies the nested list of Tiger.Expr to Exp *)
    (* val translateOrEvalExprs : Info.Info list -> Tiger.Expr list -> bool ->
                                                                      (Info.Info list * Exp) *)
    and translateOrEvalExprs (infos: Info.Info list) (es: TIG.Expr list) (nested: bool)
                                                                    : (Info.Info list * Exp) =
          let
              val (ls, lastVal)    = Utils.separateLastVal es

              (* Translate/Simplify all the expressions except the last *)
              val (infos1, stmList) = translateOrEvalList infos ls nested

              (* Translate/Simplify the last expression *)
              val (infos2, transEx) = case nested of
                      false => translateExpr infos1 lastVal
                    | true  => evalExpr      infos1 lastVal

              (* We do care about the result of the last expression *)
              val ex = unEx transEx   (* Get the Tree.Exp from the Exp *)
          in
              (infos2, Ex (T.ESEQ (T.getEseqRec (T.seq (stmList @ [getStmt ex])) T.resultTemp)))
          end

    (* Translates Tiger.Expr list to Tree.Stm list if `nested` is false,
        else simplifies the nested list of Tiger.Expr to Tree.Stm list *)
    (* val translateOrEvalList : Info.Info list -> Tiger.Expr list -> bool ->
                                                                    (Info.Info list * Tree.Stm list) *)
    and translateOrEvalList (infos: Info.Info list) (es: TIG.Expr list) (nested: bool)
                                                                  : (Info.Info list * T.Stm list) =
          let
              val initAccumulator = (infos, [])
              fun foldFun (currExp, (prevInfos, prevStmtList)) =
                  let
                      val (currInfos, transEx) = case nested of
                          (* Using previous info, translate/simplify the current expression *)
                              false => translateExpr prevInfos currExp
                            | true  => evalExpr      prevInfos currExp

                      (* We don't care about the results of these expressions *)
                      val currStmt = unNx transEx   (* Take the current statement *)
                  in
                      (currInfos, (prevStmtList @ [currStmt]))
                  end
          in
              foldl foldFun initAccumulator es
          end


    (* Compiles Tiger program to Ir program *)
    (* val compileToIR : Tiger.Prog -> Tree.Stm *)
    fun compileToIR (TIG.Expression exp) =
            let
                (* Translating the Tiger Expression *)
                val (infos, transEx) = translateExpr [Info.emptyInfo] exp

                val info =  if List.length infos = 1 then
                                (List.hd infos)
                            else
                                (raiseException ImplementationError
                                    "[compileToIR]: More than one info structure received!"
                                )

                (* Number of allocations on the stack *)
                val numAllocs = Info.getNumAllocs info

                (* Update the stack pointer based on number of allocations *)
                val optAllocStmt = case numAllocs of
                      0 => NONE
                    | _ => SOME (Frame.stackAllocStmt numAllocs)

                (* Get the final statement *)
                val e = unNx transEx
            in
                case optAllocStmt of
                      NONE           => e
                    | SOME allocStmt => T.seq [allocStmt, e]
            end
end
