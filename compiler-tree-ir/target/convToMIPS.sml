(* Contains utility functions to create a MIPS instruction statement *)
signature CONV_TO_MIPS =
sig
    val DL     : string   (* A dummy label to pass as argument *)
    val DR     : Mips.Reg (* A dummy register to pass as argument *)

    val mInst  : ('l, 't) Mips.Instruction -> ('l, 't) Mips.Stmt
    val mDir   : Mips.Directive -> 'l -> 't -> ('l, 't) Mips.Stmt

    val mLa    : 't -> 'l -> ('l, 't) Mips.Stmt
    val mLi    : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt
    val mMove  : 't -> 't -> 'l -> ('l, 't) Mips.Stmt
    val mLw    : 't -> 'l -> ('l, 't) Mips.Stmt
    val mSw    : 't -> 'l -> ('l, 't) Mips.Stmt

    val mAdd   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mAddi  : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mSub   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mSub_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mMul   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mMul_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mDiv_Q : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mDiv_QI: 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mAnd   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mAndi  : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mOr    : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mOri   : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mXor   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mXori  : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mSll   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mSll_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mSrl   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mSrl_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mSra   : 't -> 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mSra_I : 't -> 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mNeg   : 't -> 't -> 'l -> ('l, 't) Mips.Stmt

    val mBeq   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBeq_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBge   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBge_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBgeu  : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBgeu_I: 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBgt   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBgt_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBgtu  : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBgtu_I: 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBle   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBle_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBleu  : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBleu_I: 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBlt   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBlt_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBltu  : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBltu_I: 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mBne   : 't -> 't       -> 'l -> ('l, 't) Mips.Stmt
    val mBne_I : 't -> Mips.Imm -> 'l -> ('l, 't) Mips.Stmt

    val mJ     : 'l -> 't -> ('l, 't) Mips.Stmt
    val mJal   : 'l -> 't -> ('l, 't) Mips.Stmt
    val mJalr  : 't -> 'l -> ('l, 't) Mips.Stmt
    val mJr    : 't -> 'l -> ('l, 't) Mips.Stmt

    val mLabel : 'l -> 't -> ('l, 't) Mips.Stmt

    val mSyscall      : 'l -> 't                   -> ('l, 't) Mips.Stmt
    val mPrintInt     : 't -> Mips.Imm -> 't -> 'l -> ('l, 't) Mips.Stmt list
    val mPrintRegInt  : 't -> 't       -> 't -> 'l -> ('l, 't) Mips.Stmt list
    val mPrintChar    : 't -> char     -> 't -> 'l -> ('l, 't) Mips.Stmt list
    val mExit         : 't -> 'l                   -> ('l, 't) Mips.Stmt list
end

structure ConvToMIPS :> CONV_TO_MIPS =
struct
    open Mips

    val DL : string   = "__DUMMY_LABEL__"
    val DR : Mips.Reg = Mips.ZERO

    fun mInst i      = Inst i
    fun mDir  d _ _  = Dir d

    fun mLa      t  l       = Inst (DR_SL_Inst (La,   get_DR_SL_rec t  l   ))
    fun mLi      t  i  l    = Inst (DR_I_Inst  (Li,   get_DR_I_rec  t  i  l))
    fun mMove    t1 t2 l    = Inst (DR_SR_Inst (Move, get_DR_SR_rec t1 t2 l))
    fun mLw      t  l       = Inst (DR_SL_Inst (Lw,   get_DR_SL_rec t  l))
    fun mSw      t  l       = Inst (SR_DL_Inst (Sw,   get_SR_DL_rec t  l))


    fun mAdd     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Add,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mAddi    t1 t2 i  l = Inst (DR_SR_I_Inst  (Addi, get_DR_SR_I_rec  t1 t2 i  l))

    fun mSub     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Sub,   get_DR_SR_SR_rec t1 t2 t3 l))
    fun mSub_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Sub_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mMul     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Mul,   get_DR_SR_SR_rec t1 t2 t3 l))
    fun mMul_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Mul_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mDiv_Q   t1 t2 t3 l = Inst (DR_SR_SR_Inst (Div_Q,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mDiv_QI  t1 t2 i  l = Inst (DR_SR_I_Inst  (Div_QI, get_DR_SR_I_rec  t1 t2 i  l))

    fun mAnd     t1 t2 t3 l = Inst (DR_SR_SR_Inst (And,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mAndi    t1 t2 i  l = Inst (DR_SR_I_Inst  (Andi, get_DR_SR_I_rec  t1 t2 i  l))

    fun mOr      t1 t2 t3 l = Inst (DR_SR_SR_Inst (Or,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mOri     t1 t2 i  l = Inst (DR_SR_I_Inst  (Ori, get_DR_SR_I_rec  t1 t2 i  l))

    fun mXor     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Xor,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mXori    t1 t2 i  l = Inst (DR_SR_I_Inst  (Xori, get_DR_SR_I_rec  t1 t2 i  l))

    fun mSll     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Sll,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mSll_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Sll_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mSrl     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Srl,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mSrl_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Srl_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mSra     t1 t2 t3 l = Inst (DR_SR_SR_Inst (Sra,  get_DR_SR_SR_rec t1 t2 t3 l))
    fun mSra_I   t1 t2 i  l = Inst (DR_SR_I_Inst  (Sra_I, get_DR_SR_I_rec  t1 t2 i  l))

    fun mNeg     t1 t2 l    = Inst (DR_SR_Inst    (Neg, get_DR_SR_rec t1 t2 l))

    fun mBeq     t1 t2 l    = Inst (SR_SR_DL_Inst (Beq,   get_SR_SR_DL_rec t1 t2 l))
    fun mBeq_I   t  i  l    = Inst (SR_I_DL_Inst  (Beq_I, get_SR_I_DL_rec  t  i  l))

    fun mBge     t1 t2 l    = Inst (SR_SR_DL_Inst (Bge,   get_SR_SR_DL_rec t1 t2 l))
    fun mBge_I   t  i  l    = Inst (SR_I_DL_Inst  (Bge_I, get_SR_I_DL_rec  t  i  l))

    fun mBgeu    t1 t2 l    = Inst (SR_SR_DL_Inst (Bgeu,   get_SR_SR_DL_rec t1 t2 l))
    fun mBgeu_I  t  i  l    = Inst (SR_I_DL_Inst  (Bgeu_I, get_SR_I_DL_rec  t  i  l))

    fun mBgt     t1 t2 l    = Inst (SR_SR_DL_Inst (Bgt,   get_SR_SR_DL_rec t1 t2 l))
    fun mBgt_I   t  i  l    = Inst (SR_I_DL_Inst  (Bgt_I, get_SR_I_DL_rec  t  i  l))

    fun mBgtu    t1 t2 l    = Inst (SR_SR_DL_Inst (Bgtu,   get_SR_SR_DL_rec t1 t2 l))
    fun mBgtu_I  t  i  l    = Inst (SR_I_DL_Inst  (Bgtu_I, get_SR_I_DL_rec  t  i  l))

    fun mBle     t1 t2 l    = Inst (SR_SR_DL_Inst (Ble,   get_SR_SR_DL_rec t1 t2 l))
    fun mBle_I   t  i  l    = Inst (SR_I_DL_Inst  (Ble_I, get_SR_I_DL_rec  t  i  l))

    fun mBleu    t1 t2 l    = Inst (SR_SR_DL_Inst (Bleu,   get_SR_SR_DL_rec t1 t2 l))
    fun mBleu_I  t  i  l    = Inst (SR_I_DL_Inst  (Bleu_I, get_SR_I_DL_rec  t  i  l))

    fun mBlt     t1 t2 l    = Inst (SR_SR_DL_Inst (Blt,   get_SR_SR_DL_rec t1 t2 l))
    fun mBlt_I   t  i  l    = Inst (SR_I_DL_Inst  (Blt_I, get_SR_I_DL_rec  t  i  l))

    fun mBltu    t1 t2 l    = Inst (SR_SR_DL_Inst (Bltu,   get_SR_SR_DL_rec t1 t2 l))
    fun mBltu_I  t  i  l    = Inst (SR_I_DL_Inst  (Bltu_I, get_SR_I_DL_rec  t  i  l))

    fun mBne     t1 t2 l    = Inst (SR_SR_DL_Inst (Bne,   get_SR_SR_DL_rec t1 t2 l))
    fun mBne_I   t  i  l    = Inst (SR_I_DL_Inst  (Bne_I, get_SR_I_DL_rec  t  i  l))

    fun mJ       l  t       = Inst (DL_Inst (J,    get_DL_rec l t))
    fun mJal     l  t       = Inst (DL_Inst (Jal,  get_DL_rec l t))
    fun mJalr    t  l       = Inst (DR_Inst (Jalr, get_DR_rec t l))
    fun mJr      t  l       = Inst (DR_Inst (Jr,   get_DR_rec t l))

    fun mLabel   l  _       = Mips.Label l

    fun mSyscall      _  _        = Inst (ExceptionTrapInst Syscall)
    fun mPrintInt     a0 i v0 l   = [
                                        mLi a0 i l,     (* $a0 = i *)
                                        mLi v0 1 l,     (* $v0 = 1 *)
                                        mSyscall l a0   (* syscall *)
                                    ]
    fun mPrintRegInt  a0 t v0 l   = [
                                        mMove a0 t l,  (* $a0 = t *)
                                        mLi v0 1 l,    (* $v0 = 1 *)
                                        mSyscall l t   (* syscall *)
                                    ]
    fun mPrintChar    a0 c v0 l   = [
                                        mLi a0 (Char.ord c) l,   (* $a0 = ascii(c) *)
                                        mLi v0 11 l,             (* $v0 = 11 *)
                                        mSyscall l a0            (* syscall *)
                                    ]
    fun mExit         v0 l        = [
                                        mLi v0 10 l,     (* $v0 = 10 *)
                                        mSyscall l v0    (* syscall *)
                                    ]
end
