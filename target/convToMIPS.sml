(* Contains utility functions to create a MIPS instruction *)
signature CONV_TO_MIPS =
sig
    val DUMMY_STR : string   (* A dummy label to pass as argument *)

    val mAdd   : 't -> 't -> 't        -> 'l -> ('l, 't) Tiger.Instruction
    val mAddi  : 't -> 't -> Tiger.Imm -> 'l -> ('l, 't) Tiger.Instruction

    val mSub   : 't -> 't -> 't        -> 'l -> ('l, 't) Tiger.Instruction
    val mSub_I : 't -> 't -> Tiger.Imm -> 'l -> ('l, 't) Tiger.Instruction

    val mMul   : 't -> 't -> 't        -> 'l -> ('l, 't) Tiger.Instruction
    val mMul_I : 't -> 't -> Tiger.Imm -> 'l -> ('l, 't) Tiger.Instruction

    val mDiv_Q : 't -> 't -> 't        -> 'l -> ('l, 't) Tiger.Instruction
    val mDiv_QI: 't -> 't -> Tiger.Imm -> 'l -> ('l, 't) Tiger.Instruction
end

structure ConvToMIPS :> CONV_TO_MIPS =
struct
    open Mips;

    val DUMMY_STR : string = "__DUMMY_LABEL__"

    fun mAdd (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Instruction =
            DR_SR_SR_Inst(Add, {dest = a, src1 = b, src2 = c})

    fun mAddi (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Instruction =
            DR_SR_I_Inst(Addi, {dest = a, src1 = b, imm = c})

    fun mSub (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Instruction =
            DR_SR_SR_Inst(Sub, {dest = a, src1 = b, src2 = c})

    fun mSub_I (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Instruction =
            DR_SR_I_Inst(Sub_I, {dest = a, src1 = b, imm = c})

    fun mMul (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Instruction =
            DR_SR_SR_Inst(Mul, {dest = a, src1 = b, src2 = c})

    fun mMul_I (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Instruction =
            DR_SR_I_Inst(Mul_I, {dest = a, src1 = b, imm = c})

    fun mDiv_Q (a: 't) (b: 't) (c: 't) (_: 'l) : ('l, 't) Instruction =
            DR_SR_SR_Inst(Div_Q, {dest = a, src1 = b, src2 = c})

    fun mDiv_QI (a: 't) (b: 't) (c: Imm) (_: 'l) : ('l, 't) Instruction =
            DR_SR_I_Inst(Div_QI, {dest = a, src1 = b, imm = c})
end
