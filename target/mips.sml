(* Structure of the MIPS assembly language *)
structure Mips :> MIPS =
struct

    (* The 32 registers of the MIPS machine *)
    datatype Reg =
          ZERO               (* 0    : Constant 0                                      *)
        | AT                 (* 1    : Reserved for assembler                          *)
        | V0 | V1            (* 2-3  : Expression evaluation and results of a function *)
        | A0 | A1 | A2 | A3  (* 4-7  : Argument 1-4                                    *)
        | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9  (* 8-15, 24-25 : Temporary (not preserved across call) *)
        | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7            (* 16-23 : Saved temporary (preserved across call) *)
        | K0 | K1  (* 26-27 : Reserved for OS kernel                 *)
        | GP       (* 28    : Pointer to global area                 *)
        | SP       (* 29    : Stack pointer                          *)
        | FP       (* 30    : Frame pointer                          *)
        | RA       (* 31    : Return address (used by function call) *)
        (*
            The multiply and divide unit produces its result in two additional registers, `hi` and `lo`.
            These instructions move values to and from these registers.
        *)

    (* Type alias for Immediate values *)
    (* 16-bit integer *)
    type Imm = int

    (* Types of instructions based on its input arguments *)
    type ('l, 't) DL       = {dest: 'l}
    type ('l, 't) DR       = {dest: 't}
    type ('l, 't) DR_I     = {dest: 't, imm: Imm}
    type ('l, 't) DR_SL    = {dest: 't, src1: 'l}
    type ('l, 't) DR_SR    = {dest: 't, src1: 't}
    type ('l, 't) DR_SR_I  = {dest: 't, src1: 't, imm: Imm}
    type ('l, 't) DR_SR_SR = {dest: 't, src1: 't, src2: 't}
    type ('l, 't) SR_DL    = {src1: 't, dest: 'l}
    type ('l, 't) SR_SR    = {src1: 't, src2: 't}
    type ('l, 't) SR_I_DL  = {src1: 't, imm: Imm, dest: 'l}
    type ('l, 't) SR_SR_DL = {src1: 't, src2: 't, dest: 'l}

    (* The instruction datatype of the MIPS machine
        - 'l -> any label (memory address)
        - 't -> any register
        - Using the record syntax here so that it is easy to find the registers by their physical meaning of their names
        - Have documented a summary of working of instructions also in comments
        - Since the compiler only supports Integer data types, so therefore not including Floating Point instructions
    *)
    datatype ('l, 't) Instruction =
              DL_Inst           of DL_Inst__       * (('l, 't) DL      )
            | DR_Inst           of DR_Inst__       * (('l, 't) DR      )
            | DR_I_Inst         of DR_I_Inst__     * (('l, 't) DR_I    )
            | DR_SL_Inst        of DR_SL_Inst__    * (('l, 't) DR_SL   )
            | DR_SR_Inst        of DR_SR_Inst__    * (('l, 't) DR_SR   )
            | DR_SR_I_Inst      of DR_SR_I_Inst__  * (('l, 't) DR_SR_I )
            | DR_SR_SR_Inst     of DR_SR_SR_Inst__ * (('l, 't) DR_SR_SR)
            | SR_DL_Inst        of SR_DL_Inst__    * (('l, 't) SR_DL   )
            | SR_SR_Inst        of SR_SR_Inst__    * (('l, 't) SR_SR   )
            | SR_I_DL_Inst      of SR_I_DL_Inst__  * (('l, 't) SR_I_DL)
            | SR_SR_DL_Inst     of SR_SR_DL_Inst__ * (('l, 't) SR_SR_DL)
            | ExceptionTrapInst of ExceptionTrapInst__

    and DL_Inst__ =
              B               (* Branch Instruction         *)
            | Bczt            (* Branch Coprocessor z True  *)
            | Bczf            (* Branch Coprocessor z False *)
            | J               (* Jump                       *)

            (* Save the address of the next instruction in register 31 *)
            | Jal             (* Jump and Link *)

    and DR_Inst__ =
              (* Save the address of the next instruction in register 31 *)
              Jalr            (* Jump and Link Register *)

            | Jr              (* Jump Register *)
            | Mfhi            (* Move From hi  *)
            | Mflo            (* Move From lo  *)
            | Mthi            (* Move To hi    *)
            | Mtlo            (* Move To lo    *)

    and DR_I_Inst__ =
        (*Constant-Manipulation Instructions *)
            (* Move the immediate imm into register Rdest. *)
              Li              (* Load Immediate *)

            (*
                Load the lower halfword of the immediate `imm` into the upper halfword of register Rdest.
                The lower bits of the register are set to 0.
            *)
            | Lui             (* Load Upper Immediate *)

    and DR_SL_Inst__ =
        (* Load Instructions *)
              (* Load computed address, not the contents of the location, into register Rdest. *)
              La              (* Load Address *)

            (* Load the byte at address into register Rdest. *)
            | Lb              (* Load Byte          *)
            | Lbu             (* Load Unsigned Byte *)

            (* Load the 64-bit quantity at address into registers Rdest and Rdest + 1 *)
            | Ld              (* Load Double-Word *)

            (* Load the 16-bit quantity (halfword) at address into destination register *)
            | Lh              (* Load Halfword          *)
            | Lhu             (* Load Unsigned Halfword *)

            (* Load the 32-bit quantity (word) at address into destination register *)
            | Lw              (* Load Word *)

            (* Load the 32-bit quantity (word) at address into dest. register of coprocessor z (0-3) *)
            | Lwcz            (* Load Word Coprocessor *)

            (* Load the left(right) bytes from the word at the possibly-unaligned address into Rdest *)
            | Lwl             (* Load Word Left  *)
            | Lwr             (* Load Word Right *)

            (* Load at the possibly-unaligned address *)
            | Ulh             (* Unaligned Load Halfword          *)
            | Ulhu            (* Unaligned Load Halfword Unsigned *)
            | Ulw             (* Unaligned Load Word              *)

    and DR_SR_Inst__ =
              Abs             (* Absolute Value                  *)
            | Neg             (* Negate Value (with overflow)    *)
            | Negu            (* Negate Value (without overflow) *)
            | Not             (* NOT                             *)
            | Move            (* Move                            *)
            | Mfcz            (* Move From Coprocessor z         *)
            | Mtcz            (* Move To Coprocessor z           *)

    and DR_SR_I_Inst__ =
              Addi            (* Addition Immediate (with overflow) *)
            | Addiu           (* Addition Immediate (with overflow) *)
            | Andi            (* AND Immediate                      *)

            (* Put the quotient of integers from register src1 and src2 in register dest *)
            | Div_QI          (* Divide Immediate (signed, with overflow)   *)
            | Divu_QI         (* Divide Immediate (unsigned, with overflow) *)

            | Mul_I           (* Multiply Immediate (without overflow)       *)
            | Mulo_I          (* Multiply Immediate (with overflow)          *)
            | Mulou_I         (* Unsigned Immediate Multiply (with overflow) *)

            | Nor_I           (* NOR Immediate *)
            | Ori             (* OR Immediate  *)
            | Xori            (* XOR Immediate *)

            | Rem_I           (* Remainder Immediate          *)
            | Remu_I          (* Unsigned Remainder Immediate *)

            | Rol_I           (* Rotate Left Immediate  *)
            | Ror_I           (* Rotate Right Immediate *)

            | Sll_I           (* Shift Left Logical Immediate          *)
            | Sra_I           (* Sift Right Arithmetic Immediate       *)
            | Srl_I           (* Shift Right Logical Immediate         *)
            | Sub_I           (* Subtract (with overflow) Immediate    *)
            | Subu_I          (* Subtract (without overflow) Immediate *)

        (* Comparison Instructions *)
        (* Set register Rdest to 1 if (Rsrc1 op Rsrc2) satisfies, 0 otherwise *)
            | Seq_I           (* Set Equal Immediate                       *)
            | Sge_I           (* Set Greater Than Equal Immediate          *)
            | Sgeu_I          (* Set Greater Than Equal Unsigned Immediate *)
            | Sgt_I           (* Set Greater Than Immediate                *)
            | Sgtu_I          (* Set Greater Than Unsigned Immediate       *)
            | Sle_I           (* Set Less Than Equal Immediate             *)
            | Sleu_I          (* Set Less Than Equal Unsigned Immediate    *)
            | Slti            (* Set Less Than Immediate                   *)
            | Sltiu           (* Set Less Than Unsigned Immediate          *)
            | Sne_I           (* Set Not Equal Immediate                   *)

    and DR_SR_SR_Inst__ =
              Add             (* Addition (with overflow) *)
            | Addu            (* Addition (with overflow) *)
            | And             (* AND                      *)

            (* Put the quotient of integers from register src1 and src2 in register dest *)
            | Div_Q           (* Divide (signed, with overflow)   *)
            | Divu_Q          (* Divide (unsigned, with overflow) *)

            | Mul             (* Multiply (without overflow)       *)
            | Mulo            (* Multiply (with overflow)          *)
            | Mulou           (* Unsigned Multiply (with overflow) *)

            | Nor             (* NOR *)
            | Or              (* OR  *)
            | Xor             (* XOR *)

            | Rem             (* Remainder          *)
            | Remu            (* Unsigned Remainder *)

            | Rol             (* Rotate Left  *)
            | Ror             (* Rotate Right *)

            | Sll             (* Shift Left Logical              *)
            | Sllv            (* Shift Left Logical Variable     *)
            | Sra             (* Sift Right Arithmetic           *)
            | Srav            (* Shift Right Arithmetic Variable *)
            | Srl             (* Shift Right Logical             *)
            | Srlv            (* Shift Right Logical Variable    *)
            | Sub             (* Subtract (with overflow)        *)
            | Subu            (* Subtract (without overflow)     *)

        (* Comparison Instructions *)
        (* Set register Rdest to 1 if (Rsrc1 op Rsrc2) satisfies, 0 otherwise *)
            | Seq             (* Set Equal                       *)
            | Sge             (* Set Greater Than Equal          *)
            | Sgeu            (* Set Greater Than Equal Unsigned *)
            | Sgt             (* Set Greater Than                *)
            | Sgtu            (* Set Greater Than Unsigned       *)
            | Sle             (* Set Less Than Equal             *)
            | Sleu            (* Set Less Than Equal Unsigned    *)
            | Slt             (* Set Less Than                   *)
            | Sltu            (* Set Less Than Unsigned          *)
            | Sne             (* Set Not Equal                   *)

    and SR_DL_Inst__ =
              Beqz            (* Branch on Equal Zero              *)
            | Bgez            (* Branch on Greater Than Equal Zero *)
            | Bgtz            (* Branch on Greater Than Zero       *)
            | Blez            (* Branch on Less Than Equal Zero    *)
            | Bltz            (* Branch on Less Than Zero          *)
            | Bnez            (* Branch on Not Equal Zero          *)

            (* Save the address of the next instruction in register 31 *)
            | Bgezal          (* Branch on Greater Than Equal Zero And Link *)
            | Bltzal          (* Branch on Less Than Zero And Link          *)

        (* Store Instructions *)
            (* Store the low byte from register Rsrc at address *)
            | Sb              (* Store Byte *)

            (* Store the 64-bit quantity in registers Rsrc and Rsrc + 1 at address *)
            | Sd              (* Store Double-Word *)

            (* Store the low halfword/word from register Rsrc at address *)
            | Sh              (* Store Halfword *)
            | Sw              (* Store Word     *)

            (* Store the word from register Rsrc of coprocessor z at address *)
            | Swcz            (* Store Word Coprocessor *)

            (* Store the left (right) bytes from register Rsrc at the possibly-unaligned address *)
            | Swl             (* Store Word Left  *)
            | Swr             (* Store Word Right *)

            (* Store the low halfword/word from register Rsrc at the possibly-unaligned address *)
            | Ush             (* Unaligned Store Halfword *)
            | Usw             (* Unaligned Store Word     *)

    and SR_SR_Inst__ =
            (*
                Divide the contents of the two registers.
                Leave the quotient in register `lo` and the remainder in register `hi`
            *)
              Div             (* Divide (signed)   *)
            | Divu            (* Divide (unsigned) *)

            (*
                Multiply the contents of the two registers.
                Leave the low-order word of the product in register `lo`, and
                the high-word in register `hi`
            *)
            | Mult            (* Multiply          *)
            | Multu           (* Unsigned Multiply *)

    and SR_I_DL_Inst__ =
              Beq_I           (* Branch on Equal                 *)
            | Bge_I           (* Branch on Greater Than Equal    *)
            | Bgeu_I          (* Branch on GTE Unsigned          *)
            | Bgt_I           (* Branch on Greater Than          *)
            | Bgtu_I          (* Branch on Greater Than Unsigned *)
            | Ble_I           (* Branch on Less Than Equal       *)
            | Bleu_I          (* Branch on LTE Unsigned          *)
            | Blt_I           (* Branch on Less Than             *)
            | Bltu_I          (* Branch on Less Than Unsigned    *)
            | Bne_I           (* Branch on Not Equal             *)

    and SR_SR_DL_Inst__ =
              Beq             (* Branch on Equal                 *)
            | Bge             (* Branch on Greater Than Equal    *)
            | Bgeu            (* Branch on GTE Unsigned          *)
            | Bgt             (* Branch on Greater Than          *)
            | Bgtu            (* Branch on Greater Than Unsigned *)
            | Ble             (* Branch on Less Than Equal       *)
            | Bleu            (* Branch on LTE Unsigned          *)
            | Blt             (* Branch on Less Than             *)
            | Bltu            (* Branch on Less Than Unsigned    *)
            | Bne             (* Branch on Not Equal             *)

    and ExceptionTrapInst__ =
        (* Exception and Trap Instructions *)
              (* Restore the Status register *)
              Rfe             (* Return From Exception *)

            (* Register $v0 contains the system call number provided by SPIM *)
            | Syscall         (* System Call *)

            (* Cause exception n. Exception 1 is reserved for the debugger. *)
            | Break of int    (* Break *)

            (* Do nothing *)
            | Nop             (* No operation *)


    (* Assembler Directives of the MIPS machine*)
    datatype Directive =
              Align  of int
            | Ascii  of string
            | Asciiz of string
            | Byte   of int list
            | Data
            | Extern of {sym: string, size: int}
            | Globl  of string
            | Half   of int list
            | Kdata
            | Ktext
            | Space  of int
            | Text
            | Word   of int list

    (* Statements of the MIPS machine: the instructions and the assembler directives *)
    datatype ('l, 't) Stmt =  Inst  of ('l, 't) Instruction
                            | Dir   of Directive
                            | Label of string

    (* MIPS program *)
    type ('l, 't) Prog = ('l, 't) Stmt list

    (*=========================================================================================*)
    (* Utility functions for mapping different types of records based on input *)
    fun mapRecDL       (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DL)       : ('lp, 'tp) DL       =
        {dest = f (#dest r)}

    fun mapRecDR       (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DR)       : ('lp, 'tp) DR       =
        {dest = g (#dest r)}

    fun mapRecDR_I     (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DR_I)     : ('lp, 'tp) DR_I     =
        {dest = g (#dest r), imm = #imm r}

    fun mapRecDR_SL    (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DR_SL)    : ('lp, 'tp) DR_SL    =
        {dest = g (#dest r), src1 = f (#src1 r)}

    fun mapRecDR_SR    (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DR_SR)    : ('lp, 'tp) DR_SR    =
        {dest = g (#dest r), src1 = g (#src1 r)}

    fun mapRecDR_SR_I  (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DR_SR_I)  : ('lp, 'tp) DR_SR_I  =
        {dest = g (#dest r), src1 = g (#src1 r), imm = #imm r}

    fun mapRecDR_SR_SR (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) DR_SR_SR) : ('lp, 'tp) DR_SR_SR =
        {dest = g (#dest r), src1 = g (#src1 r), src2 = g (#src2 r)}

    fun mapRecSR_DL    (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) SR_DL)    : ('lp, 'tp) SR_DL    =
        {src1 = g (#src1 r), dest = f (#dest r)}

    fun mapRecSR_SR    (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) SR_SR)    : ('lp, 'tp) SR_SR    =
        {src1 = g (#src1 r), src2 = g (#src2 r)}

    fun mapRecSR_I_DL  (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) SR_I_DL)  : ('lp, 'tp) SR_I_DL  =
        {src1 = g (#src1 r), imm = #imm r, dest = f (#dest r)}

    fun mapRecSR_SR_DL (f: 'l -> 'lp) (g: 't -> 'tp) (r: ('l, 't) SR_SR_DL) : ('lp, 'tp) SR_SR_DL =
        {src1 = g (#src1 r), src2 = g (#src2 r), dest = f (#dest r)}
    (*=========================================================================================*)

    (* Used to convert instructions from one parametric type to another *)
    (* mapInst: ('l -> 'lp) -> ('t -> 'tp) -> ('l, 't) Instruction -> ('lp, 'tp) Instruction *)
    fun mapInst (f: 'l -> 'lp) (g: 't -> 'tp) (inst: ('l, 't) Instruction) = case inst of
          DL_Inst         (i: DL_Inst__,         r: ('l, 't) DL)       =>
                DL_Inst (i, mapRecDL f g r)

        | DR_Inst         (i: DR_Inst__,         r: ('l, 't) DR)       =>
                DR_Inst (i, mapRecDR f g r)

        | DR_I_Inst        (i: DR_I_Inst__,      r: ('l, 't) DR_I)     =>
                DR_I_Inst (i, mapRecDR_I f g r)

        | DR_SL_Inst        (i: DR_SL_Inst__,    r: ('l, 't) DR_SL)    =>
                DR_SL_Inst (i, mapRecDR_SL f g r)

        | DR_SR_Inst        (i: DR_SR_Inst__,    r: ('l, 't) DR_SR)    =>
                DR_SR_Inst (i, mapRecDR_SR f g r)

        | DR_SR_I_Inst      (i: DR_SR_I_Inst__,  r: ('l, 't) DR_SR_I)  =>
                DR_SR_I_Inst (i, mapRecDR_SR_I f g r)

        | DR_SR_SR_Inst     (i: DR_SR_SR_Inst__, r: ('l, 't) DR_SR_SR) =>
                DR_SR_SR_Inst (i, mapRecDR_SR_SR f g r)

        | SR_DL_Inst        (i: SR_DL_Inst__,    r: ('l, 't) SR_DL)    =>
                SR_DL_Inst (i, mapRecSR_DL f g r)

        | SR_SR_Inst        (i: SR_SR_Inst__,    r: ('l, 't) SR_SR)    =>
                SR_SR_Inst (i, mapRecSR_SR f g r)

        | SR_I_DL_Inst      (i: SR_I_DL_Inst__,  r: ('l, 't) SR_I_DL)  =>
                SR_I_DL_Inst (i, mapRecSR_I_DL f g r)

        | SR_SR_DL_Inst     (i: SR_SR_DL_Inst__, r: ('l, 't) SR_SR_DL) =>
                SR_SR_DL_Inst (i, mapRecSR_SR_DL f g r)

        | ExceptionTrapInst (i: ExceptionTrapInst__)                   =>
                ExceptionTrapInst i

    (* Used to convert statements from one parametric type to another *)
    (* mapStmt: ('l -> 'lp) -> ('t -> 'tp) -> ('l, 't) Stmt -> ('lp, 'tp) Stmt *)
    fun mapStmt (f: 'l -> 'lp) (g: 't -> 'tp) (stmt: ('l, 't) Stmt) =
        case stmt of
              Inst  i => Inst (mapInst f g i)
            | Dir   d => Dir d
            | Label l => Label l

    (* Used to convert program from one parametric type to another *)
    (* mapProg: ('l -> 'lp) -> ('t -> 'tp) -> ('l, 't) Prog -> ('lp, 'tp) Prog *)
    fun mapProg (f: 'l -> 'lp) (g: 't -> 'tp) (prog: ('l, 't) Prog) = map (mapStmt f g) prog
end
