signature PRETTY_MIPS =
sig
    (* Pretty prints the register *)
    val prettyReg: Mips.Reg -> string

    (* Pretty prints the immediate value *)
    val prettyImm: Mips.Imm -> string

    (* Pretty prints the instruction *)
    val prettyInst: (string, string) Mips.Instruction -> string

    (* Pretty prints the assembler directive *)
    val prettyDir: Mips.Directive -> string

    (* Pretty prints the statement *)
    val prettyStmt: (string, string) Mips.Stmt -> string

    (* Maps and pretty prints the instruction *)
    val prettyMapInst: ('l -> string) -> ('t -> string) -> ('l, 't) Mips.Instruction -> string

    (* Maps and pretty prints the statement *)
    val prettyMapStmt: ('l -> string) -> ('t -> string) -> ('l, 't) Mips.Stmt -> string
end

structure PrettyMips :> PRETTY_MIPS =
struct
    open Mips;

    (* Some utility functions *)
    fun indent     str = "    " ^ str
    fun addNewline str = str ^ "\n"

    val indentAndAddNewline = indent o addNewline

    (* Pretty prints the register *)
    (* prettyReg: Reg -> string *)
    fun prettyReg (reg : Reg) : string =
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

    (* Pretty prints the immediate value *)
    (* prettyImm: Imm -> string *)
    fun prettyImm (imm : Imm) : string = if (imm >= 0) then (Int.toString imm)
                                         else "-" ^ (Int.toString (~imm))

    (*=========================================================================================*)
    (* Utility functions for pretty printing different types of records based on input *)
    fun prettyRecDL       (r: (string, string) DL)       =
        (#dest r)

    fun prettyRecDR       (r: (string, string) DR)       =
        (#dest r)

    fun prettyRecDR_I     (r: (string, string) DR_I)     =
        (#dest r) ^ ", " ^ (prettyImm (#imm r))

    fun prettyRecDR_SL    (r: (string, string) DR_SL)    =
        (#dest r) ^ ", " ^ (#src1 r)

    fun prettyRecDR_SR    (r: (string, string) DR_SR)    =
        (#dest r) ^ ", " ^ (#src1 r)

    fun prettyRecDR_SR_I  (r: (string, string) DR_SR_I)  =
        (#dest r) ^ ", " ^ (#src1 r) ^ ", " ^ (prettyImm (#imm r))

    fun prettyRecDR_SR_SR (r: (string, string) DR_SR_SR) =
        (#dest r) ^ ", " ^ (#src1 r) ^ ", " ^ (#src2 r)

    fun prettyRecSR_DL    (r: (string, string) SR_DL)    =
        (#src1 r) ^ ", " ^ (#dest r)

    fun prettyRecSR_SR    (r: (string, string) SR_SR)    =
        (#src1 r) ^ ", " ^ (#src2 r)

    fun prettyRecSR_I_DL  (r: (string, string) SR_I_DL)  =
        (#src1 r) ^ ", " ^ (prettyImm (#imm r)) ^ ", " ^ (#dest r)

    fun prettyRecSR_SR_DL (r: (string, string) SR_SR_DL) =
        (#src1 r) ^ ", " ^ (#src2 r) ^ ", " ^ (#dest r)
    (*=========================================================================================*)

    (* Pretty prints the instruction *)
    (* prettyInst: (string, string) Instruction -> string *)
    fun prettyInst (inst : (string, string) Instruction) = case inst of
        DL_Inst (i: DL_Inst__, r: (string, string) DL) =>
            let
                val iStr = case i of
                      B    => "b"
                    | Bczt => "bczt"
                    | Bczf => "bczf"
                    | J    => "j"
                    | Jal  => "jal"
            in
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDL r))
            end

        | DR_Inst (i: DR_Inst__, r: (string, string) DR) =>
            let
                val iStr = case i of
                      Jalr => "jalr"
                    | Jr   => "jr"
                    | Mfhi => "mfhi"
                    | Mflo => "mflo"
                    | Mthi => "mthi"
                    | Mtlo => "mtlo"
            in
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDR r))
            end

        | DR_I_Inst (i: DR_I_Inst__, r: (string, string) DR_I) =>
            let
                val iStr = case i of
                      Li  => "li"
                    | Lui => "lui"
            in
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDR_I r))
            end

        | DR_SL_Inst (i: DR_SL_Inst__, r: (string, string) DR_SL) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDR_SL r))
            end

        | DR_SR_Inst (i: DR_SR_Inst__, r: (string, string) DR_SR) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDR_SR r))
            end

        | DR_SR_I_Inst (i: DR_SR_I_Inst__, r: (string, string) DR_SR_I) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDR_SR_I r))
            end

        | DR_SR_SR_Inst (i: DR_SR_SR_Inst__, r: (string, string) DR_SR_SR) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecDR_SR_SR r))
            end

        | SR_DL_Inst (i: SR_DL_Inst__, r: (string, string) SR_DL) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecSR_DL r))
            end

        | SR_SR_Inst (i: SR_SR_Inst__, r: (string, string) SR_SR) =>
            let
                val iStr = case i of
                      Div   => "div"
                    | Divu  => "divu"
                    | Mult  => "mult"
                    | Multu => "multu"
            in
                indentAndAddNewline (iStr ^ " " ^ (prettyRecSR_SR r))
            end

        | SR_I_DL_Inst (i: SR_I_DL_Inst__, r: (string, string) SR_I_DL) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecSR_I_DL r))
            end

        | SR_SR_DL_Inst (i: SR_SR_DL_Inst__, r: (string, string) SR_SR_DL) =>
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
                indentAndAddNewline (iStr ^ " " ^ (prettyRecSR_SR_DL r))
            end

        | ExceptionTrapInst (i: ExceptionTrapInst__) =>
            let
                val iStr = case i of
                      Rfe     => "rfe"
                    | Syscall => "syscall"
                    | Break n => "break " ^ (Int.toString n)
                    | Nop     => "nop"
            in
                indentAndAddNewline iStr
            end

    (* Converts the given list of integers to comma separated list of string values *)
    (* intListToCSVString: int list -> string *)
    fun intListToCSVString ([] : int list) : string = ""
      | intListToCSVString ([x]: int list) : string = Int.toString x
      | intListToCSVString (x :: xs) : string = (Int.toString x) ^ ", " ^ (intListToCSVString xs)

    (* Pretty prints the assembler directive *)
    (* prettyDir: Directive -> string *)
    fun prettyDir (dir : Directive) : string =
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
            indentAndAddNewline ("." ^ strDir)
        end

    (* Pretty prints the statement *)
    (* prettyStmt: (string, string) Stmt -> string *)
    fun prettyStmt (stm : (string, string) Stmt) : string =
        case stm of
              Inst  i => (prettyInst i)
            | Dir   d => (prettyDir d)
            | Label l => addNewline (l ^ ":")

    (* Maps and pretty prints the instruction *)
    (* prettyMapInst: ('l -> string) -> ('t -> string) -> ('l, 't) Instruction -> string *)
    fun prettyMapInst (f: 'l -> string) (g: 't -> string) (x: ('l, 't) Instruction) = prettyInst (mapInst f g x)

    (* Maps and pretty prints the statement *)
    (* prettyMapStmt: ('l -> string) -> ('t -> string) -> ('l, 't) Stmt -> string *)
    fun prettyMapStmt (f: 'l -> string) (g: 't -> string) (x: ('l, 't) Stmt) = prettyStmt (mapStmt f g x)
end
