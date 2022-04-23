(* Structure for MipsBasicBlock instructions *)
structure MipsInst : INST =
struct
    open Mips

    (* Type of Instruction for Blocks in case of Mips Program *)
    type t = (string, Reg) Stmt

    (* Tells whether a Mips Instruction is JumpLike or not *)
    fun isJumpInst (inst : (string, Reg) Instruction) : bool = case inst of
          DL_Inst       _ => true
        | SR_I_DL_Inst  _ => true
        | SR_SR_DL_Inst _ => true

        | DR_Inst (i: DR_Inst__, _: (string, Reg) DR) => (case i of
                                                              Jalr => true
                                                            | Jr   => true
                                                            | _    => false
                                                        )

        | SR_DL_Inst (i: SR_DL_Inst__, _: (string, Reg) SR_DL) => (case i of
                                                                          Beqz   => true
                                                                        | Bgez   => true
                                                                        | Bgtz   => true
                                                                        | Blez   => true
                                                                        | Bltz   => true
                                                                        | Bnez   => true
                                                                        | Bgezal => true
                                                                        | Bltzal => true
                                                                        | _      => false
                                                                    )

        | _ => false

    (* Tells whether a Mips statement is JumpLike or not *)
    fun isJumpLike (s : t) : bool =
        case s of
              Inst  i => isJumpInst i
            | Dir   d => false
            | Label l => false

    (* Tells whether a Mips statement can be a potential Target or not *)
    fun isTarget (s : t) : bool =
        case s of
              Inst  i => false
            | Dir   d => false
            | Label l => true
end

(* MipsBasicBlock structure *)
structure MipsBasicBlocks = BasicBlocks (MipsInst)
