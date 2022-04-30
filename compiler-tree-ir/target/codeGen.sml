signature CODE_GEN =
sig
    val generateMipsProg : Tree.Stm list -> (string, Mips.Reg) Mips.Prog
end

(*
    -   We are translating in such a way, that the CALL statements will always be
        the child of EXP(), and we will store their return value in the `resultTemp` register
        after they have been successfully called.
        Hence, there will be no nested calls.
        If there are consecutive calls, then there values will be overwritten which is also
        correct as there is only one `returnValue` register in MIPS.
    -   We will only handle those cases which we generated while translating and canonizing and
        the rest will raise exceptions.
*)
structure CodeGen :> CODE_GEN =
struct
    (* structures used *)
    structure T   = Tree
    structure CTM = ConvToMIPS
    structure PT  = PrettyTree
    structure PM  = PrettyMips

    (* Raising exception and printing the error message *)
    exception RestrictionFailed of string
    exception Unimplemented     of string

    fun raiseException (ex: string -> exn) (msg: string) =
                    Utils.throwErr ex ("[codeGen.sml]:" ^ msg ^ "\n\n")

    (* Converts a temporary special value to Mips register  *)
    (* val specialValToReg : Temp.value -> Mips.Reg *)
    fun specialValToReg (value : Temp.value) = case Temp.valToInt value of
              0  => Mips.T0
            | 1  => Mips.T1
            | 2  => Mips.T2
            | 3  => Mips.T3
            | 4  => Mips.T4
            | 5  => Mips.T5
            | 6  => Mips.T6
            | 7  => Mips.T7
            | 8  => Mips.T8
            | 9  => Mips.T9
            | 10 => Mips.FP
            | 11 => Mips.SP
            | 12 => Mips.RA
            | 13 => Mips.V0
            | v  => (raiseException RestrictionFailed
                        ("[specialValToReg]: \"" ^ (Int.toString v) ^ "\" is not a valid special value")
                    )

    (* Extracts the offset and the register from the expression inside Tree.MEM *)
    (* val getOffsetAndRegFromMem : Tree.Exp -> int * Mips.Reg *)
    fun getOffsetAndRegFromMem (mem : T.Exp) = case mem of
            (T.BINOP {left, oper, right}) =>
                (case (left, oper, right) of
                    (* This is only possible expression we implemented while translating/canonizing *)
                        (T.TEMP l, T.PLUS, T.CONST r) => (r, specialValToReg l)
                        | _  => (raiseException RestrictionFailed
                                    ("[getOffsetAndRegFromMem]: \"" ^ (PT.prettyTreeExp mem) ^
                                                    "\" is not an implemented expression for Tree.BINOP used with Tree.MEM")
                                )
                )
            | _ => (raiseException RestrictionFailed
                        ("[getOffsetAndRegFromMem]: \"" ^ (PT.prettyTreeExp mem) ^
                                                    "\" is not an implemented expression for Tree.MEM")
                    )

    (* Get the register address string for the MIPS instruction *)
    (* val getRegAddr : int -> Mips.Reg -> string *)
    fun getRegAddr (offset : int) (reg : Mips.Reg) =
            let
                val regStr = PM.prettyReg reg           (* stringified form of the Mips.Reg *)
                (* stringified form the offset number *)
                val offsetStr = if (offset >= 0) then (Int.toString offset)
                                else ("-" ^ (Int.toString (~offset)))
            in
                offsetStr ^ "(" ^ regStr ^ ")"
            end

    (* Converts the Canonicalized Tree IR statement into Mips Program *)
    (* val convStmToProg : Tree.Stm -> (string, Mips.Reg) Mips.Prog *)
    fun convStmToProg (T.MOVE {lhs, rhs}) =
            (case (lhs, rhs) of
                  (T.TEMP t,  T.CONST c) => [CTM.mLi (specialValToReg t) c CTM.DL]
                | (T.TEMP t1, T.TEMP t2) => [CTM.mMove (specialValToReg t1) (specialValToReg t2) CTM.DL]
                | (T.TEMP t,  T.BINOP {left, oper, right}) =>
                        let
                            val (arg1, arg2) = case (left, right) of  (* left and right val *)
                                                    (T.TEMP argTemp1, T.TEMP argTemp2) =>
                                                                    (specialValToReg argTemp1, specialValToReg argTemp2)
                                                    | _ => (raiseException RestrictionFailed
                                                                        ("[convStmToProg]: LHS \"" ^ (PT.prettyTreeExp left) ^
                                                                            "\" and RHS \"" ^ (PT.prettyTreeExp right) ^
                                                                                    "\" of BINOP are not registers")
                                                            )
                            val resultReg = specialValToReg t    (* result register *)
                        in
                            case oper of
                                  T.PLUS    => [CTM.mAdd   resultReg arg1 arg2 CTM.DL]
                                | T.MINUS   => [CTM.mSub   resultReg arg1 arg2 CTM.DL]
                                | T.MUL     => [CTM.mMul   resultReg arg1 arg2 CTM.DL]
                                | T.DIV     => [CTM.mDiv_Q resultReg arg1 arg2 CTM.DL]
                                | T.AND     => [CTM.mAnd   resultReg arg1 arg2 CTM.DL]
                                | T.OR      => [CTM.mOr    resultReg arg1 arg2 CTM.DL]
                                | T.LSHIFT  => [CTM.mSll   resultReg arg1 arg2 CTM.DL]
                                | T.RSHIFT  => [CTM.mSrl   resultReg arg1 arg2 CTM.DL]
                                | T.ARSHIFT => [CTM.mSra   resultReg arg1 arg2 CTM.DL]
                                | T.XOR     => [CTM.mXor   resultReg arg1 arg2 CTM.DL]
                        end
                | (T.TEMP t, T.MEM mem)  =>
                        let
                            val (offset, reg) = getOffsetAndRegFromMem mem
                            (* Extracting the offset and the register from the expression inside Tree.MEM *)
                            val resultReg = specialValToReg t   (* result register *)
                        in
                            [CTM.mLw resultReg (getRegAddr offset reg)]
                        end
                | (T.TEMP t, T.CALL _)   =>
                        let
                            val resultReg = specialValToReg t                   (* result register *)
                            val returnReg = specialValToReg Temp.returnValue  (* return value register *)
                        in
                            [CTM.mMove resultReg returnReg CTM.DL]
                        end
                | (T.MEM mem, T.TEMP t)  =>
                        let
                            val (offset, reg) = getOffsetAndRegFromMem mem
                            (* Extracting the offset and the register from the expression inside Tree.MEM *)
                            val regFrom = specialValToReg t   (* source register *)
                        in
                            [CTM.mSw regFrom (getRegAddr offset reg)]
                        end
                | _ => (raiseException RestrictionFailed
                                ("[convStmToProg]: LHS: \"" ^ (PT.prettyTreeExp lhs) ^ "\" and RHS: \""
                                                    ^ (PT.prettyTreeExp rhs) ^ "\" of MOVE are not as expected.")
                        )
            )
      | convStmToProg (T.EXP e) =
            (case e of
                (T.CALL {func, args}) =>
                    (* Except for `print` and `println` functions,
                        argument list will be empty as we are allocating everything on stack *)
                    (case (func, args) of
                        (T.NAME lab, [T.TEMP t]) =>
                            let
                                val funcName = Temp.prettyLabel lab  (* The function label *)
                                val printReg = specialValToReg t     (* The register value to print *)

                                (* Statements to print a newline character in case of `println` *)
                                val printlnStmts = case funcName of
                                      "print"   => []
                                    | "println" => CTM.mPrintChar Mips.A0 #"\n" Mips.V0 CTM.DL
                                    | _ => (raiseException RestrictionFailed
                                                ("[convStmToProg]: \"" ^ funcName ^
                                                    "\" is not allowed to have register arguments.")
                                            )
                            in
                                (CTM.mPrintRegInt Mips.A0 printReg Mips.V0 CTM.DL) @ printlnStmts
                            end
                        | (T.NAME lab, []) =>
                            let
                                val funcName = Temp.prettyLabel lab  (* The function label *)
                            in
                                [CTM.mJal funcName CTM.DR]
                            end
                        | _ => (raiseException RestrictionFailed
                                    ("[convStmToProg]: \"" ^ (PT.prettyTreeExp func) ^
                                        "\" is not valid!")
                                )
                    )
                | _ => (raiseException RestrictionFailed
                                ("[convStmToProg]: \"" ^ (PT.prettyTreeExp e) ^
                                        "\" is not an implemented expression for Tree.EXP")
                        )
            )
      | convStmToProg (T.JUMP {addr, labs}) =
            (case addr of
                  (T.NAME l) => [CTM.mJ (Temp.prettyLabel l) CTM.DR]
                | (T.TEMP t) => [CTM.mJr (specialValToReg t) CTM.DL]
                | _          => (raiseException RestrictionFailed
                                    ("[convStmToProg]: Target \"" ^ (PT.prettyTreeExp addr)
                                                        ^ "\" of JUMP can either be a label or a register.")
                                )
            )
      | convStmToProg (T.CJUMP {left, oper, right, tLab, fLab}) =
            let
                val (reg1, reg2) = case (left, right) of  (* left and right val *)
                                        (T.TEMP argTemp1, T.TEMP argTemp2) => (specialValToReg argTemp1, specialValToReg argTemp2)
                                        | _ => (raiseException RestrictionFailed
                                                        ("[convStmToProg]: LHS \"" ^ (PT.prettyTreeExp left) ^
                                                                        "\" and RHS \"" ^ (PT.prettyTreeExp right) ^
                                                                                            "\" of BINOP are not registers")
                                                )

                val trueLabelStr  = Temp.prettyLabel tLab   (* True result label *)
                val falseLabelStr = Temp.prettyLabel fLab   (* False result label *)

                val instConstructor = case oper of  (* Instruction constructor *)
                          T.EQ  => CTM.mBeq
                        | T.NE  => CTM.mBne
                        | T.LT  => CTM.mBlt
                        | T.GT  => CTM.mBgt
                        | T.LE  => CTM.mBle
                        | T.GE  => CTM.mBge
                        | T.ULT => CTM.mBltu
                        | T.ULE => CTM.mBleu
                        | T.UGT => CTM.mBgtu
                        | T.UGE => CTM.mBgeu
            in
                [instConstructor reg1 reg2 trueLabelStr, CTM.mJ falseLabelStr CTM.DR]
            end
      | convStmToProg (T.SEQ _)   = (raiseException RestrictionFailed
                                            ("[convStmToProg]: SEQ should not be present after canonization.")
                                    )
      | convStmToProg (T.LABEL l) = [CTM.mLabel (Temp.prettyLabel l) CTM.DR]


    (* Converts the Canonicalized Tree IR statements into Mips Program *)
    (* val convStmsToProg : Tree.Stm list -> (string, Mips.Reg) Mips.Prog *)
    fun convStmsToProg []        = []
      | convStmsToProg (x :: xs) = (convStmToProg x) @ (convStmsToProg xs)


    (* Converts the Canonicalized Tree IR statements into Mips Program *)
    (* val generateMipsProg : Tree.Stm list -> (string, Mips.Reg) Mips.Prog *)
    fun generateMipsProg (stms : T.Stm list) =
            let
                val mainProg = convStmsToProg stms  (* Translated Tree IR code as Mips.Prog *)
                val initFP   = [CTM.mMove Mips.FP Mips.SP CTM.DL] (* Initializing FP register *)

                (* Default header for the MIPS assembly
                        .data
                        .text
                        .globl main
                    main:
                *)
                val headerStmts = [
                                    CTM.mDir Mips.Data CTM.DL CTM.DR,
                                    CTM.mDir Mips.Text CTM.DL CTM.DR,
                                    CTM.mDir (Mips.Globl "main") CTM.DL CTM.DR,
                                    CTM.mLabel "main" CTM.DR
                                ]

                (* The exit program statements *)
                val exitStmts = CTM.mExit Mips.V0 CTM.DL
            in
                (headerStmts @ initFP @ mainProg @ exitStmts)
            end
end
