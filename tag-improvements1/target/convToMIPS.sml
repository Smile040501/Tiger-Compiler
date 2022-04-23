(* Contains utility functions to create a MIPS instruction statement *)
signature CONV_TO_MIPS =
sig
    val DUMMY_STR : string   (* A dummy label to pass as argument *)

    val mInst  : ('l, 't) Mips.Instruction -> ('l, 't) Mips.Stmt
    val mDir   : Mips.Directive -> 'l -> 't -> ('l, 't) Mips.Stmt

    val mLa    : 't -> 'l -> ('l, 't) Mips.Stmt
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

    val mBeq   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBeq_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBge   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBge_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBgt   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBgt_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBle   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBle_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBlt   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBlt_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBne   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBne_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mJ  : 'l -> 't -> ('l, 't) Mips.Stmt
    val mJr : 't -> 'l -> ('l, 't) Mips.Stmt

    val mSyscall : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt list

    val mLabel : 'l -> 't -> ('l, 't) Mips.Stmt
end

structure ConvToMIPS :> CONV_TO_MIPS =
struct
    open Mips

    val DUMMY_STR : string = "__DUMMY_LABEL__"

    fun mInst i      = Inst i
    fun mDir  d _ _  = Dir d

    fun mLa      t  l       = Inst (DR_SL_Inst (La,   get_DR_SL_rec t  l   ))
    fun mLi      t  i  l    = Inst (DR_I_Inst  (Li,   get_DR_I_rec  t  i  l))
    fun mMove    t1 t2 l    = Inst (DR_SR_Inst (Move, get_DR_SR_rec t1 t2 l))

    fun mAdd     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Add,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mAddi    t1 t2 i  l = Inst (DR_SR_I_Inst  (Addi, get_DR_SR_I_rec  t1 t2 i  l))

    fun mSub     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Sub,   get_DR_SR_SR_rec t1 t2 t3 l))
    fun mSub_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Sub_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mMul     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Mul,   get_DR_SR_SR_rec t1 t2 t3 l))
    fun mMul_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Mul_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mDiv_Q   t1 t2 t3 l = Inst (DR_SR_SR_Inst (Div_Q,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mDiv_QI  t1 t2 i  l = Inst (DR_SR_I_Inst  (Div_QI, get_DR_SR_I_rec  t1 t2 i  l))

    fun mNeg     t1 t2 l    = Inst (DR_SR_Inst    (Neg, get_DR_SR_rec t1 t2 l))

    fun mBeq     t1 t2 l    = Inst (SR_SR_DL_Inst (Beq,   get_SR_SR_DL_rec t1 t2 l))
    fun mBeq_I   t  i  l    = Inst (SR_I_DL_Inst  (Beq_I, get_SR_I_DL_rec  t  i  l))

    fun mBge     t1 t2 l    = Inst (SR_SR_DL_Inst (Bge,   get_SR_SR_DL_rec t1 t2 l))
    fun mBge_I   t  i  l    = Inst (SR_I_DL_Inst  (Bge_I, get_SR_I_DL_rec  t  i  l))

    fun mBgt     t1 t2 l    = Inst (SR_SR_DL_Inst (Bgt,   get_SR_SR_DL_rec t1 t2 l))
    fun mBgt_I   t  i  l    = Inst (SR_I_DL_Inst  (Bgt_I, get_SR_I_DL_rec  t  i  l))

    fun mBle     t1 t2 l    = Inst (SR_SR_DL_Inst (Ble,   get_SR_SR_DL_rec t1 t2 l))
    fun mBle_I   t  i  l    = Inst (SR_I_DL_Inst  (Ble_I, get_SR_I_DL_rec  t  i  l))

    fun mBlt     t1 t2 l    = Inst (SR_SR_DL_Inst (Blt,   get_SR_SR_DL_rec t1 t2 l))
    fun mBlt_I   t  i  l    = Inst (SR_I_DL_Inst  (Blt_I, get_SR_I_DL_rec  t  i  l))

    fun mBne     t1 t2 l    = Inst (SR_SR_DL_Inst (Bne,   get_SR_SR_DL_rec t1 t2 l))
    fun mBne_I   t  i  l    = Inst (SR_I_DL_Inst  (Bne_I, get_SR_I_DL_rec  t  i  l))

    fun mJ       l  t       = Inst (DL_Inst (J,  get_DL_rec l t))
    fun mJr      t  l       = Inst (DR_Inst (Jr, get_DR_rec t l))

    fun mSyscall t  i  l    = [mLi t i l, Inst (ExceptionTrapInst Syscall)]

    fun mLabel   l  _       = Mips.Label l
end
