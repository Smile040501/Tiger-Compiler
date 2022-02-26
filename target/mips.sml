structure MIPS =
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

    (* The instruction datatype of the MIPS machine *)
    (*
        'l -> any label (memory address)
        't -> any register
    *)
    (*
        - Using the record syntax here so that it is easy to find the registers by their physical meaning of their names
        - Have documented a summary of working of instructions also in comments
        - Since the compiler only supports Integer data types, so therefore not including Floating Point instructions
    *)
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
    type ('l, 't) SR_SI_DL = {src1: 't, imm: Imm, dest: 'l}
    type ('l, 't) SR_SR_DL = {src1: 't, src2: 't, dest: 'l}

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
            | SR_SI_DL_Inst     of SR_SI_DL_Inst__ * (('l, 't) SR_SI_DL)
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

    and SR_SI_DL_Inst__ =
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
    datatype ('l, 't) Stmt =  Inst of ('l, 't) Instruction
                            | Dir of Directive


    (* Prints the register *)
    fun printReg (reg : Reg) : string =
        let
            val strReg = case reg of
                  ZERO => "zero"
                | AT   => "at"
                | V0   => "v0"
                | V1   => "v1"
                | A0   => "a0"
                | A1   => "a1"
                | A2   => "a2"
                | A3   => "a3"
                | T0   => "t0"
                | T1   => "t1"
                | T2   => "t2"
                | T3   => "t3"
                | T4   => "t4"
                | T5   => "t5"
                | T6   => "t6"
                | T7   => "t7"
                | T8   => "t8"
                | T9   => "t9"
                | S0   => "s0"
                | S1   => "s1"
                | S2   => "s2"
                | S3   => "s3"
                | S4   => "s4"
                | S5   => "s5"
                | S6   => "s6"
                | S7   => "s7"
                | K0   => "k0"
                | K1   => "k1"
                | GP   => "gp"
                | SP   => "sp"
                | FP   => "fp"
                | RA   => "ra"
        in
            "$" ^ strReg
        end

    (* Prints the immediate value *)
    fun printImm (imm : Imm) : string = Int.toString imm

    (*=========================================================================================*)
    (* Utility functions for printing different types of records based on input *)
    fun printRecDL (r: (string, Reg) DL) : string = (#dest r)
    fun printRecDR (r: (string, Reg) DR) : string = (printReg (#dest r))
    fun printRecDR_I (r: (string, Reg) DR_I) : string = (printReg (#dest r)) ^ ", " ^ (printImm (#imm r))
    fun printRecDR_SL (r: (string, Reg) DR_SL) : string = (printReg (#dest r)) ^ ", " ^ (#src1 r)
    fun printRecDR_SR (r: (string, Reg) DR_SR) : string = (printReg (#dest r)) ^ ", " ^ (printReg (#src1 r))
    fun printRecDR_SR_I (r: (string, Reg) DR_SR_I) : string = (printReg (#dest r)) ^ ", " ^ (printReg (#src1 r)) ^ ", " ^ (printImm (#imm r))
    fun printRecDR_SR_SR (r: (string, Reg) DR_SR_SR) : string = (printReg (#dest r)) ^ ", " ^ (printReg (#src1 r)) ^ ", " ^ (printReg (#src2 r))
    fun printRecSR_DL (r: (string, Reg) SR_DL) : string = (printReg (#src1 r)) ^ ", " ^ (#dest r)
    fun printRecSR_SR (r: (string, Reg) SR_SR) : string = (printReg (#src1 r)) ^ ", " ^ (printReg (#src2 r))
    fun printRecSR_SI_DL (r: (string, Reg) SR_SI_DL) : string = (printReg (#src1 r)) ^ ", " ^ (printImm (#imm r)) ^ ", " ^ (#dest r)
    fun printRecSR_SR_DL (r: (string, Reg) SR_SR_DL) : string = (printReg (#src1 r)) ^ ", " ^ (printReg (#src2 r)) ^ ", " ^ (#dest r)
    (*=========================================================================================*)

    (* Print the instructions when the labels are strings and registers are actual MIPS registers *)
    fun printInst (inst : (string, Reg) Instruction) = case inst of
        DL_Inst (i: DL_Inst__, r: (string, Reg) DL) =>
            let
                val iStr = case i of
                      B    => "b"
                    | Bczt => "bczt"
                    | Bczf => "bczf"
                    | J    => "j"
                    | Jal  => "jal"
            in
                iStr ^ " " ^ (printRecDL r)
            end

        | DR_Inst (i: DR_Inst__, r: (string, Reg) DR) =>
            let
                val iStr = case i of
                      Jalr => "jalr"
                    | Jr   => "jr"
                    | Mfhi => "mfhi"
                    | Mflo => "mflo"
                    | Mthi => "mthi"
                    | Mtlo => "mtlo"
            in
                iStr ^ " " ^ (printRecDR r)
            end

        | DR_I_Inst (i: DR_I_Inst__, r: (string, Reg) DR_I) =>
            let
                val iStr = case i of
                      Li  => "li"
                    | Lui => "lui"
            in
                iStr ^ " " ^ (printRecDR_I r)
            end

        | DR_SL_Inst (i: DR_SL_Inst__, r: (string, Reg) DR_SL) =>
            let
                val iStr = case i of
                      La   => "la"
                    | Lb   => "lb"
                    | Lbu  => "lbu"
                    | Ld   => "ld"
                    | Lh   => "lh"
                    | Lhu  => "lhu"
                    | Lw   => "lw"
                    | Lwcz => "lwcz"
                    | Lwl  => "lwl"
                    | Lwr  => "lwr"
                    | Ulh  => "ulh"
                    | Ulhu => "ulhu"
                    | Ulw  => "ulw"
            in
                iStr ^ " " ^ (printRecDR_SL r)
            end

        | DR_SR_Inst (i: DR_SR_Inst__, r: (string, Reg) DR_SR) =>
            let
                val iStr = case i of
                      Abs  => "abs"
                    | Neg  => "neg"
                    | Negu => "negu"
                    | Not  => "not"
                    | Move => "move"
                    | Mfcz => "mfcz"
                    | Mtcz => "mtcz"
            in
                iStr ^ " " ^ (printRecDR_SR r)
            end

        | DR_SR_I_Inst (i: DR_SR_I_Inst__, r: (string, Reg) DR_SR_I) =>
            let
                val iStr = case i of
                      Addi    => "addi"
                    | Addiu   => "addiu"
                    | Andi    => "andi"
                    | Div_QI  => "div"
                    | Divu_QI => "divu"
                    | Mul_I   => "mul"
                    | Mulo_I  => "mulo"
                    | Mulou_I => "mulou"
                    | Nor_I   => "nor"
                    | Ori     => "ori"
                    | Xori    => "xori"
                    | Rem_I   => "rem"
                    | Remu_I  => "remu"
                    | Rol_I   => "rol"
                    | Ror_I   => "ror"
                    | Sll_I   => "sll"
                    | Sra_I   => "sra"
                    | Srl_I   => "srl"
                    | Sub_I   => "sub"
                    | Subu_I  => "subu"
                    | Seq_I   => "seq"
                    | Sge_I   => "sge"
                    | Sgeu_I  => "sgeu"
                    | Sgt_I   => "sgt"
                    | Sgtu_I  => "sgtu"
                    | Sle_I   => "sle"
                    | Sleu_I  => "sleu"
                    | Slti    => "slti"
                    | Sltiu   => "sltiu"
                    | Sne_I   => "sne"
            in
                iStr ^ " " ^ (printRecDR_SR_I r)
            end

        | DR_SR_SR_Inst (i: DR_SR_SR_Inst__, r: (string, Reg) DR_SR_SR) =>
            let
                val iStr = case i of
                      Add   => "add"
                    | Addu  => "addu"
                    | And   => "and"
                    | Div_Q => "div"
                    | Divu_Q=> "divu"
                    | Mul   => "mul"
                    | Mulo  => "mulo"
                    | Mulou => "mulou"
                    | Nor   => "nor"
                    | Or    => "or"
                    | Xor   => "xor"
                    | Rem   => "rem"
                    | Remu  => "remu"
                    | Rol   => "rol"
                    | Ror   => "ror"
                    | Sll   => "sll"
                    | Sllv  => "sllv"
                    | Sra   => "sra"
                    | Srav  => "srav"
                    | Srl   => "srl"
                    | Srlv  => "srlv"
                    | Sub   => "sub"
                    | Subu  => "subu"
                    | Seq   => "seq"
                    | Sge   => "sge"
                    | Sgeu  => "sgeu"
                    | Sgt   => "sgt"
                    | Sgtu  => "sgtu"
                    | Sle   => "sle"
                    | Sleu  => "sleu"
                    | Slt   => "slt"
                    | Sltu  => "sltu"
                    | Sne   => "sne"
            in
                iStr ^ " " ^ (printRecDR_SR_SR r)
            end

        | SR_DL_Inst (i: SR_DL_Inst__, r: (string, Reg) SR_DL) =>
            let
                val iStr = case i of
                      Beqz   => "beqz"
                    | Bgez   => "bgez"
                    | Bgtz   => "bgtz"
                    | Blez   => "blez"
                    | Bltz   => "bltz"
                    | Bnez   => "bnez"
                    | Bgezal => "bgezal"
                    | Bltzal => "bltzal"
                    | Sb     => "sb"
                    | Sd     => "sd"
                    | Sh     => "sh"
                    | Sw     => "sw"
                    | Swcz   => "swcz"
                    | Swl    => "swl"
                    | Swr    => "swr"
                    | Ush    => "ush"
                    | Usw    => "usw"
            in
                iStr ^ " " ^ (printRecSR_DL r)
            end

        | SR_SR_Inst (i: SR_SR_Inst__, r: (string, Reg) SR_SR) =>
            let
                val iStr = case i of
                      Div   => "div"
                    | Divu  => "divu"
                    | Mult  => "mult"
                    | Multu => "multu"
            in
                iStr ^ " " ^ (printRecSR_SR r)
            end

        | SR_SI_DL_Inst (i: SR_SI_DL_Inst__, r: (string, Reg) SR_SI_DL) =>
            let
                val iStr = case i of
                      Beq_I  => "beq"
                    | Bge_I  => "bge"
                    | Bgeu_I => "bgeu"
                    | Bgt_I  => "bgt"
                    | Bgtu_I => "bgtu"
                    | Ble_I  => "ble"
                    | Bleu_I => "bleu"
                    | Blt_I  => "blt"
                    | Bltu_I => "bltu"
                    | Bne_I  => "bne"
            in
                iStr ^ " " ^ (printRecSR_SI_DL r)
            end

        | SR_SR_DL_Inst (i: SR_SR_DL_Inst__, r: (string, Reg) SR_SR_DL) =>
            let
                val iStr = case i of
                      Beq  => "beq"
                    | Bge  => "bge"
                    | Bgeu => "bgeu"
                    | Bgt  => "bgt"
                    | Bgtu => "bgtu"
                    | Ble  => "ble"
                    | Bleu => "bleu"
                    | Blt  => "blt"
                    | Bltu => "bltu"
                    | Bne  => "bne"
            in
                iStr ^ " " ^ (printRecSR_SR_DL r)
            end

        | ExceptionTrapInst (i: ExceptionTrapInst__) =>
            case i of
                  Rfe     => "rfe"
                | Syscall => "syscall"
                | Break n => "break " ^ (Int.toString n)
                | Nop     => "nop"

    (* Converts the given list of integers to comma separated list of string values *)
    fun intListToCSVString ([] : int list) : string = ""
      | intListToCSVString ([x]: int list) : string = Int.toString x
      | intListToCSVString (x :: xs) : string = (Int.toString x) ^ ", " ^ (intListToCSVString xs)

    (* Prints the assembler directive *)
    fun printDir (dir : Directive) : string =
        let
            val strDir = case dir of
                  Align  i => "align "  ^ (Int.toString i)
                | Ascii  s => "ascii " ^ s
                | Asciiz s => "asciiz " ^ s
                | Byte   l => "byte "   ^ (intListToCSVString l)
                | Data     => "data"
                | Extern r => "extern " ^ (#sym r) ^ " " ^ (Int.toString (#size r))
                | Globl  s => "globl "  ^ s
                | Half   l => "half "   ^ (intListToCSVString l)
                | Kdata    => "kdata"
                | Ktext    => "ktext"
                | Space  i => "space "  ^ (Int.toString i)
                | Text     => "text"
                | Word   l => "word "   ^ (intListToCSVString l)
        in
            "." ^ strDir
        end

    (* Prints the statement *)
    fun printStmt (stm : (string, Reg) Stmt) : string =
        case stm of
              Inst i => (printInst i)
            | Dir  d => (printDir d)

end
