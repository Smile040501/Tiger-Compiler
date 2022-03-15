(* Contains utility functions to create a MIPS instruction statement *)
signature CONV_TO_MIPS =
sig
    val DUMMY_STR : string   (* A dummy label to pass as argument *)

    val mapInstToStmt  : ('l, 't) Mips.Instruction -> ('l, 't) Mips.Stmt
    val mapDirToStmt   : 'l -> 't -> Mips.Directive -> ('l, 't) Mips.Stmt

    val mLi    : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt
    val mMove  : 't -> 't -> 'l -> ('l, 't) Mips.Stmt

    val mAdd   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mAddi  : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mSub   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mSub_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mMul   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mMul_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mDiv_Q : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mDiv_QI: 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mNeg   : 't -> 't -> 'l -> ('l, 't) Mips.Stmt

    val mSyscall : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt list

    val mBgt   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBgt_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mJ  : 'l -> 't -> ('l, 't) Mips.Stmt
    val mJr : 't -> 'l -> ('l, 't) Mips.Stmt

    val mLabel : 'l -> 't -> ('l, 't) Mips.Stmt
end

structure ConvToMIPS :> CONV_TO_MIPS =
struct
    open Mips

    val DUMMY_STR : string = "__DUMMY_LABEL__"

    fun mapInstToStmt i = Inst i
    fun mapDirToStmt   (_: 'l) (_: 't) (d: Directive) : ('l, 't) Stmt = Dir d

    fun mLi (a: 't) (b: Imm) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_I_Inst (Li, {dest = a, imm = b}))

    fun mMove (a : 't) (b : 't) (_ : 'l) : ('l, 't) Stmt =
            Inst (DR_SR_Inst (Move, {dest = a, src1 = b}))

    fun mAdd (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_SR_Inst (Add, {dest = a, src1 = b, src2 = c}))

    fun mAddi (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_I_Inst (Addi, {dest = a, src1 = b, imm = c}))

    fun mSub (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_SR_Inst (Sub, {dest = a, src1 = b, src2 = c}))

    fun mSub_I (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_I_Inst (Sub_I, {dest = a, src1 = b, imm = c}))

    fun mMul (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_SR_Inst (Mul, {dest = a, src1 = b, src2 = c}))

    fun mMul_I (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_I_Inst (Mul_I, {dest = a, src1 = b, imm = c}))

    fun mDiv_Q (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_SR_Inst (Div_Q, {dest = a, src1 = b, src2 = c}))

    fun mDiv_QI (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_I_Inst (Div_QI, {dest = a, src1 = b, imm = c}))

    fun mNeg (a: 't) (b: 't) (_: 'l) : ('l, 't) Stmt =
            Inst (DR_SR_Inst (Neg, {dest = a, src1 = b}))

    fun mSyscall (a: 't) (b: Imm) (c:'l) : ('l, 't) Stmt list =
            [mLi a b c, Inst (ExceptionTrapInst Syscall)]

    fun mBgt (a: 't) (b: 't) (c: 'l) : ('l, 't) Stmt =
            Inst (SR_SR_DL_Inst (Bgt, {src1 = a, src2 = b, dest = c}))

    fun mBgt_I (a: 't) (b: Imm) (c: 'l) : ('l, 't) Stmt =
            Inst (SR_I_DL_Inst (Bgt_I, {src1 = a, imm = b, dest = c}))

    fun mJ (a : 'l) (_ : 't) : ('l, 't) Stmt =
            Inst (DL_Inst (J, {dest = a}))

    fun mJr (a : 't) (_ : 'l) : ('l, 't) Stmt =
            Inst (DR_Inst (Jr, {dest = a}))

    fun mLabel (a : 'l) (_ : 't) : ('l, 't) Stmt = Mips.Label a
end
