structure MIPS =
struct

    (* The 32 registers of the MIPS machine *)
    datatype Reg =
        zero                 (* 0    : Constant 0                                      *)
        | at                 (* 1    : Reserved for assembler                          *)
        | v0 | v1            (* 2-3  : Expression evaluation and results of a function *)
        | a0 | a1 | a2 | a3  (* 4-7  : Argument 1-4                                    *)
        | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | t9  (* 8-15, 24-25 : Temporary (not preserved across call) *)
        | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7  (* 16-23 : Saved temporary (preserved across call) *) 
        | k0 | k1  (* 26-27 : Reserved for OS kernel                 *)
        | gp       (* 28    : Pointer to global area                 *)
        | sp       (* 29    : Stack pointer                          *)
        | fp       (* 30    : Frame pointer                          *)
        | ra       (* 31    : Return address (used by function call) *)

    (* Type alias for Immediate values *)
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
    datatype  ('l,'t) Instruction = 
    (* Arithimatic and Logical Instructions *)
        Abs of {dest: 't, src1: 't}  (* Absolute Value *)

        | Add   of {dest: 't, src1: 't, src2: 't}  (* Addition (with overflow)           *)
        | Addi  of {dest: 't, src1: 't, imm: Imm}  (* Addition Immediate (with overflow) *)
        | Addu  of {dest: 't, src1: 't, src2: 't}  (* Addition (with overflow)           *)
        | Addiu of {dest: 't, src1: 't, imm: Imm}  (* Addition Immediate (with overflow) *)

        | And   of {dest: 't, src1: 't, src2: 't}  (* AND           *)
        | Andi  of {dest: 't, src1: 't, imm: Imm}  (* AND Immediate *)

        (*
            Divide the contents of the two registers.
            Leave the quotient in register `lo` and the remainder in register `hi`
        *)
        | Div   of {src1: 't, src2: 't}  (* Divide (signed)   *)
        | Divu  of {src1: 't, src2: 't}  (* Divide (unsigned) *)
        
        (* Put the quotient of integers from register src1 and src2 in register dest   *)
        | Divq  of {dest: 't, src1: 't, src2: 't}  (* Divide (signed, with overflow)   *)
        | Divuq of {dest: 't, src1: 't, src2: 't}  (* Divide (unsigned, with overflow) *)

        | Mul   of {dest: 't, src1: 't, src2: 't}  (* Multiply (without overflow)       *)
        | Mulo  of {dest: 't, src1: 't, src2: 't}  (* Multiply (with overflow)          *)
        | Mulou of {dest: 't, src1: 't, src2: 't}  (* Unsigned Multiply (with overflow) *)

        (*
            Multiply the contents of the two registers.
            Leave the low-order word of the product in register `lo`, and
            the high-word in register `hi`
        *)
        | Mult  of {src1: 't, src2: 't}  (* Multiply          *)
        | Multu of {src1: 't, src2: 't}  (* Unsigned Multiply *)

        | Neg  of {dest: 't, src1: 't}            (* Negate Value (with overflow)    *)
        | Negu of {dest: 't, src1: 't}            (* Negate Value (without overflow) *)
        | Nor  of {dest: 't, src1: 't, src2: 't}  (* NOR                             *)
        | Not  of {dest: 't, src1: 't}            (* NOT                             *)
        | Or   of {dest: 't, src1: 't, src2: 't}  (* OR                              *)
        | Ori  of {dest: 't, src1: 't, imm: Imm}  (* OR Immediate                    *)

        | Rem  of {dest: 't, src1: 't, src2: 't}  (* Remainder          *)
        | Remu of {dest: 't, src1: 't, src2: 't}  (* Unsigned Remainder *)

        | Rol  of {dest: 't, src1: 't, src2: 't}  (* Rotate Left  *)
        | Ror  of {dest: 't, src1: 't, src2: 't}  (* Rotate Right *)

        | Sll  of {dest: 't, src1: 't, src2: 't}  (* Shift Left Logical              *)
        | Sllv of {dest: 't, src1: 't, src2: 't}  (* Shift Left Logical Variable     *)
        | Sra  of {dest: 't, src1: 't, src2: 't}  (* Sift Right Arithmetic           *)
        | Srav of {dest: 't, src1: 't, src2: 't}  (* Shift Right Arithmetic Variable *)
        | Srl  of {dest: 't, src1: 't, src2: 't}  (* Shift Right Logical             *)
        | Srlv of {dest: 't, src1: 't, src2: 't}  (* Shift Right Logical Variable    *)

        | Sub  of {dest: 't, src1: 't, src2: 't}  (* Subtract (with overflow)    *)
        | Subu of {dest: 't, src1: 't, src2: 't}  (* Subtract (without overflow) *)

        | Xor  of {dest: 't, src1: 't, src2: 't}  (* XOR           *)
        | Xori of {dest: 't, src1: 't, imm: Imm}  (* XOR Immediate *)

    (*Constant-Manipulation Instructions *)
        | Li   of {dest: 't, imm: Imm}  (* Load Immediate       *)
        | Lui  of {dest: 't, imm: Imm}  (* Load Upper Immediate *)

    (*Comparison Instructions*)
        | Seq   of {dest: 't, src1: 't, src2: 't}  (* Set Equal                        *)
        | Sge   of {dest: 't, src1: 't, src2: 't}  (* Set Greater Than Equal           *)
        | Sgeu  of {dest: 't, src1: 't, src2: 't}  (* Set Greater Than Equal Unsigned  *)
        | Sgt   of {dest: 't, src1: 't, src2: 't}  (* Set Greater Than                 *)
        | Sgtu  of {dest: 't, src1: 't, src2: 't}  (* Set Greater Than Unsigned        *)
        | Sle   of {dest: 't, src1: 't, src2: 't}  (* Set Less Than Equal              *)
        | Sleu  of {dest: 't, src1: 't, src2: 't}  (* Set Less Than Equal Unsigned     *)
        | Slt   of {dest: 't, src1: 't, src2: 't}  (* Set Less Than                    *)
        | Slti  of {dest: 't, src1: 't, imm: Imm}  (* Set Less Than Immediate          *) 
        | Sltu  of {dest: 't, src1: 't, src2: 't}  (* Set Less Than Unsigned           *)
        | Sltiu of {dest: 't, src1: 't, imm: Imm}  (* Set Less Than Unsigned Immediate *)
        | Sne   of {dest: 't, src1: 't, src2: 't}  (* Set Not Equal                    *)

    (*Branch and Jump Instructions*)
        | B      of {dest: 'l}                      (* Branch Instruction         *)
        | Bczt   of {dest: 'l}                      (* Branch Coprocessor z True  *)
        | Bczf   of {dest: 'l}                      (* Branch Coprocessor z False *)

        | Beq    of {src1: 't, src2: 't, dest: 'l}  (* Branch on Equal                   *)
        | Beqz   of {src1: 't, dest: 'l}            (* Branch on Equal Zero              *)
        | Bge    of {src1: 't, src2: 't, dest: 'l}  (* Branch on Greater Than Equal      *)
        | Bgeu   of {src1: 't, src2: 't, dest: 'l}  (* Branch on GTE Unsigned            *)
        | Bgez   of {src1: 't, dest: 'l}            (* Branch on Greater Than Equal Zero *)
        | Bgt    of {src1: 't, src2: 't, dest: 'l}  (* Branch on Greater Than            *)
        | Bgtu   of {src1: 't, src2: 't, dest: 'l}  (* Branch on Greater Than Unsigned   *)
        | Bgtz   of {src1: 't, dest: 'l}            (* Branch on Greater Than Zero       *)
        | Ble    of {src1: 't, src2: 't, dest: 'l}  (* Branch on Less Than Equal         *)
        | Bleu   of {src1: 't, src2: 't, dest: 'l}  (* Branch on LTE Unsigned            *)
        | Blez   of {src1: 't, dest: 'l}            (* Branch on Less Than Equal Zero    *)

        (* Save the address of the next instruction in register 31 *)
        | Bgezal of {src1: 't, dest: 'l}            (* Branch on Greater Than Equal Zero And Link *)
        | Bltzal of {src1: 't, dest: 'l}            (* Branch on Less Than Zero And Link          *)

        | Blt    of {src1: 't, src2: 't, dest: 'l}  (* Branch on Less Than               *)
        | Bltu   of {src1: 't, src2: 't, dest: 'l}  (* Branch on Less Than Unsigned      *)
        | Bltz   of {src1: 't, dest: 'l}            (* Branch on Less Than Zero          *)
        | Bne    of {src1: 't, src2: 't, dest: 'l}  (* Branch on Not Equal               *)
        | Bnez   of {src1: 't, dest: 'l}            (* Branch on Not Equal Zero          *)

        | J      of {dest: 'l}  (* Jump                   *)
        | Jal    of {dest: 'l}  (* Jump and Link          *)
        | Jalr   of {dest: 't}  (* Jump and Link Register *)
        | Jr     of {dest: 't}  (* Jump Register          *)

    (* Load Instructions *)
        | La    of {dest: 't, src1: 'l}  (* Load Address                     *)
        | Lb    of {dest: 't, src1: 'l}  (* Load Byte                        *)
        | Lbu   of {dest: 't, src1: 'l}  (* Load Unsigned Byte               *)

        (* Load the 64-bit quantity at address into registers Rdest and Rdest + 1 *)
        | Ld    of {dest: 't, src1: 'l}  (* Load Double-Word                 *)

        (* Load the 16-bit quantity (halfword) at address into destination register *)
        | Lh    of {dest: 't, src1: 'l}  (* Load Halfword                    *)
        | Lhu   of {dest: 't, src1: 'l}  (* Load Unsigned Halfword           *)

        (* Load the 32-bit quantity (word) at address into destination register *)
        | Lw    of {dest: 't, src1: 'l}  (* Load Word                        *)

        (* Load the 32-bit quantity (word) at address into dest. register of coprocessor z (0-3) *)
        | Lwcz  of {dest: 't, src1: 'l}  (* Load Word Coprocessor            *)

        (* Load the left (right) bytes from the word at the possibly-unaligned address into Rdest *)
        | Lwl   of {dest: 't, src1: 'l}  (* Load Word Left                   *)
        | Lwr   of {dest: 't, src1: 'l}  (* Load Word Right                  *)


        | Ulh   of {dest: 't, src1: 'l}  (* Unaligned Load Halfword          *)
        | Ulhu  of {dest: 't, src1: 'l}  (* Unaligned Load Halfword Unsigned *)
        | Ulw   of {dest: 't, src1: 'l}  (* Unaligned Load Word              *)

    (* Store Instructions *)
        (* Store the low byte from register Rsrc at address *)
        | Sb   of {src1: 't, dest: 'l}  (* Store Byte               *)

        (* Store the 64-bit quantity in registers Rsrc and Rsrc + 1 at address *)
        | Sd   of {src1: 't, dest: 'l}  (* Store Double-Word        *)

        (* Store the low halfword/word from register Rsrc at address *)
        | Sh   of {src1: 't, dest: 'l}  (* Store Halfword           *)
        | Sw   of {src1: 't, dest: 'l}  (* Store Word               *)

        (* Store the word from register Rsrc of coprocessor z at address *)
        | Swcz of {src1: 't, dest: 'l}  (* Store Word Coprocessor   *)

        (* Store the left (right) bytes from register Rsrc at the possibly-unaligned address *)
        | Swl  of {src1: 't, dest: 'l}  (* Store Word Left          *)
        | Swr  of {src1: 't, dest: 'l}  (* Store Word Right         *)

        (* Store the low halfword/word from register Rsrc at the possibly-unaligned address *)
        | Ush  of {src1: 't, dest: 'l}  (* Unaligned Store Halfword *)
        | Usw  of {src1: 't, dest: 'l}  (* Unaligned Store Word     *)
    
    (* Data Movement Instructions *)
        | Move of {dest: 't, src1: 't}  (* Move         *)
        | Mfhi of {dest: 't}            (* Move From hi *)
        | Mflo of {dest: 't}            (* Move From lo *)
        | Mthi of {dest: 't}            (* Move To hi   *)
        | Mtlo of {dest: 't}            (* Move To lo   *)

    (* Exception and Trap Instructions *)
        | Rfe      (* Return From Exception *)
        | Syscall  (* System Call           *)
        | Break    (* Break                 *)
        | Nop      (* No operation          *)

    (* Assembler Directives of the MIPS machine*)
    datatype Directive =
            Align    of int
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
    datatype ('l, 't) Stmt = Inst of ('l, 't) Instruction
                            | Dir of Directive

    
    (* Prints the registers *)
    fun prReg (reg : Reg) : string = case reg of
                    zero => "$zero"
                    | at => "$at"
                    | v0 => "$v0"
                    | v1 => "$v1"
                    | a0 => "$a0"
                    | a1 => "$a1"
                    | a2 => "$a2"
                    | a3 => "$a3"
                    | t0 => "$t0"
                    | t1 => "$t1"
                    | t2 => "$t2"
                    | t3 => "$t3"
                    | t4 => "$t4"
                    | t5 => "$t5"
                    | t6 => "$t6"
                    | t7 => "$t7"
                    | t8 => "$t8"
                    | t9 => "$t9"
                    | s0 => "$s0"
                    | s1 => "$s1"
                    | s2 => "$s2"
                    | s3 => "$s3"
                    | s4 => "$s4"
                    | s5 => "$s5"
                    | s6 => "$s6"
                    | s7 => "$s7"
                    | k0 => "$k0"
                    | k1 => "$k1"
                    | gp => "$gp"
                    | sp => "$sp"
                    | fp => "$fp"
                    | ra => "$ra"

    (* Prints the immediate value *)
    fun prImm (imm : Imm) : string = Int.toString imm

    (*=========================================================================================*)
    (* Utility functions for printing different types of records based on input *)
    fun prRecordDS (r: {dest: Reg, src1: Reg}) = (prReg (#dest r)) ^ "," ^ (prReg (#src1 r))
    fun prRecordDSL (r: {dest: Reg, src1: string}) = (prReg (#dest r)) ^ "," ^ (#src1 r)

    fun prRecordDSS (r : {dest: Reg, src1: Reg, src2: Reg}) = (prReg (#dest r)) ^ "," ^ (prReg (#src1 r)) ^ "," ^ (prReg (#src2 r))
    fun prRecordDSI (r: {dest: Reg, src1: Reg, imm: Imm}) = (prReg (#dest r)) ^ "," ^ (prReg (#src1 r)) ^ "," ^ (prImm (#imm r))
    fun prRecordDI (r: {dest: Reg, imm: Imm}) = (prReg (#dest r)) ^ "," ^ (prImm (#imm r))
    fun prRecordSS (r: {src1: Reg, src2: Reg}) = (prReg (#src1 r)) ^ "," ^ (prReg (#src2 r))

    fun prRecordD (r: {dest: Reg}) = (prReg (#dest r))
    fun prRecordDL (r: {dest: string}) = (#dest r)
    
    fun prRecordSDL (r: {src1: Reg, dest: string}) = (prReg (#src1 r)) ^ "," ^ (#dest r)
    fun prRecordSSDL (r: {src1: Reg, src2: Reg, dest: string}) = (prReg (#src1 r)) ^ "," ^ (prReg (#src2 r)) ^ "," ^ (#dest r)
    (*=========================================================================================*)

    (* Print the instructions when the labels are strings and registers are actual MIPS registers *)
    fun prInst (inst : (string, Reg) Instruction) = case inst of
    (* Arithmetic Instructions *)
        Abs r      => "abs " ^ (prRecordDS r)
        | Add r    => "add " ^ (prRecordDSS r)
        | Addi r   => "addi " ^ (prRecordDSI r)
        | Addu r   => "addu " ^ (prRecordDSS r)
        | Addiu r  => "addiu " ^ (prRecordDSI r)
        | And r    => "and " ^ (prRecordDSS r)
        | Andi r   => "andi " ^ (prRecordDSI r)
        | Div r    => "div " ^ (prRecordSS r)
        | Divu r   => "divu " ^ (prRecordSS r)
        | Divq r   => "div " ^ (prRecordDSS r)
        | Divuq r  => "divu " ^ (prRecordDSS r)
        | Mul r    => "mul " ^ (prRecordDSS r)
        | Mulo r   => "mulo " ^ (prRecordDSS r)
        | Mulou r  => "mulou " ^ (prRecordDSS r)
        | Mult r   => "mult " ^ (prRecordSS r)
        | Multu r  => "multu " ^ (prRecordSS r)
        | Neg r    => "neg " ^ (prRecordDS r)
        | Negu r   => "negu " ^ (prRecordDS r)
        | Nor r    => "nor " ^ (prRecordDSS r)
        | Not r    => "not " ^ (prRecordDS r)
        | Or r     => "or " ^ (prRecordDSS r)
        | Ori r    => "ori " ^ (prRecordDSI r)
        | Rem r    => "rem " ^ (prRecordDSS r)
        | Remu r   => "remu " ^ (prRecordDSS r)
        | Rol r    => "rol " ^ (prRecordDSS r)
        | Ror r    => "ror " ^ (prRecordDSS r)
        | Sll r    => "sll " ^ (prRecordDSS r)
        | Sllv r   => "sllv " ^ (prRecordDSS r)
        | Sra r    => "sra " ^ (prRecordDSS r)
        | Srav r   => "srav " ^ (prRecordDSS r)
        | Srl r    => "srl " ^ (prRecordDSS r)
        | Srlv r   => "srlv " ^ (prRecordDSS r)
        | Sub r    => "sub " ^ (prRecordDSS r)
        | Subu r   => "subu " ^ (prRecordDSS r)
        | Xor r    => "xor " ^ (prRecordDSS r)
        | Xori r   => "xori " ^ (prRecordDSI r)
    (*Constant-Manipulation Instructions *)
        | Li r     => "li " ^ (prRecordDI r)
        | Lui r    => "lui " ^ (prRecordDI r)
    (*Comparison Instructions*)
        | Seq r    => "seq " ^ (prRecordDSS r)
        | Sge r    => "sge " ^ (prRecordDSS r)
        | Sgeu r   => "sgeu " ^ (prRecordDSS r)
        | Sgt r    => "sgt " ^ (prRecordDSS r)
        | Sgtu r   => "sgtu " ^ (prRecordDSS r)
        | Sle r    => "sle " ^ (prRecordDSS r)
        | Sleu r   => "sleu " ^ (prRecordDSS r)
        | Slt r    => "slt " ^ (prRecordDSS r)
        | Slti r   => "slti " ^ (prRecordDSI r)
        | Sltu r   => "sltu " ^ (prRecordDSS r)
        | Sltiu r  => "sltiu " ^ (prRecordDSI r)
        | Sne r    => "sne " ^ (prRecordDSS r)
    (*Branch and Jump Instructions*)
        | B r      => "b " ^ (prRecordDL r)
        | Bczt r   => "bczt " ^ (prRecordDL r)
        | Bczf r   => "bczf " ^ (prRecordDL r)
        | Beq r    => "beq " ^ (prRecordSSDL r)
        | Beqz r   => "beqz " ^ (prRecordSDL r)
        | Bge r    => "bge " ^ (prRecordSSDL r)
        | Bgeu r   => "bgeu " ^ (prRecordSSDL r)
        | Bgez r   => "bgez " ^ (prRecordSDL r)
        | Bgt r    => "bgt " ^ (prRecordSSDL r)
        | Bgtu r   => "bgtu " ^ (prRecordSSDL r)
        | Bgtz r   => "bgtz " ^ (prRecordSDL r)
        | Ble r    => "ble " ^ (prRecordSSDL r)
        | Bleu r   => "bleu " ^ (prRecordSSDL r)
        | Blez r   => "blez " ^ (prRecordSDL r)
        | Bgezal r => "bgezal " ^ (prRecordSDL r)
        | Bltzal r => "bltzal " ^ (prRecordSDL r)
        | Blt r    => "blt " ^ (prRecordSSDL r)
        | Bltu r   => "bltu " ^ (prRecordSSDL r)
        | Bltz r   => "bltz " ^ (prRecordSDL r)
        | Bne r    => "bne " ^ (prRecordSSDL r)
        | Bnez r   => "bnez " ^ (prRecordSDL r)
        | J r      => "j " ^ (prRecordDL r)
        | Jal r    => "jal " ^ (prRecordDL r)
        | Jalr r   => "jalr " ^ (prRecordD r)
        | Jr r     => "jr " ^ (prRecordD r)
    (* Load Instructions *)
        | La r     => "la " ^ (prRecordDSL r)
        | Lb r     => "lb " ^ (prRecordDSL r)
        | Lbu r    => "lbu " ^ (prRecordDSL r)
        | Ld r     => "ld " ^ (prRecordDSL r)
        | Lh r     => "lh " ^ (prRecordDSL r)
        | Lhu r    => "lhu " ^ (prRecordDSL r)
        | Lw r     => "lw " ^ (prRecordDSL r)
        | Lwcz r   => "lwcz " ^ (prRecordDSL r)
        | Lwl r    => "lwl " ^ (prRecordDSL r)
        | Lwr r    => "lwr " ^ (prRecordDSL r)
        | Ulh r    => "ulh " ^ (prRecordDSL r)
        | Ulhu r   => "ulhu " ^ (prRecordDSL r)
        | Ulw r    => "ulw " ^ (prRecordDSL r)
    (* Store Instructions *)
        | Sb r     => "sb " ^ (prRecordSDL r)
        | Sd r     => "sd " ^ (prRecordSDL r)
        | Sh r     => "sh " ^ (prRecordSDL r)
        | Sw r     => "sw " ^ (prRecordSDL r)
        | Swcz r   => "swcz " ^ (prRecordSDL r)
        | Swl r    => "swl " ^ (prRecordSDL r)
        | Swr r    => "swr " ^ (prRecordSDL r)
        | Ush r    => "ush " ^ (prRecordSDL r)
        | Usw r    => "usw " ^ (prRecordSDL r)
    (* Data Movement Instructions *)
        | Move r   => "move " ^ (prRecordDS r)
        | Mfhi r   => "mfhi " ^ (prRecordD r)
        | Mflo r   => "mflo " ^ (prRecordD r)
        | Mthi r   => "mthi " ^ (prRecordD r)
        | Mtlo r   => "mtlo " ^ (prRecordD r)
    (* Exception and Trap Instructions *)
        | Rfe      => "rfe"
        | Syscall  => "syscall"
        | Break    => "break"
        | Nop      => "nop"

        

    (* Converts the given list of integers to comma separated list of string values *)
    fun intListToCSVString ([] : int list) : string = ""
      | intListToCSVString ([x]: int list) : string = Int.toString x
      | intListToCSVString (x :: xs) : string = (Int.toString x) ^ ", " ^ (intListToCSVString xs)
    
    (* Prints the assembler directives *)
    fun prDir (dir : Directive) : string = case dir of
            Align    i => ".align " ^ (Int.toString i)
            | Ascii  s => ".a.scii " ^ s
            | Asciiz s => ".asciiz " ^ s
            | Byte   l => ".byte " ^ (intListToCSVString l)
            | Data     => ".data"
            | Extern r => ".extern " ^ (#sym r) ^ " " ^ (Int.toString (#size r))
            | Globl  s => ".globl " ^ s
            | Half   l => ".half " ^ (intListToCSVString l)
            | Kdata    => ".kdata"
            | Ktext    => ".ktext"
            | Space  i => ".space " ^ (Int.toString i)
            | Text     => ".text"
            | Word   l => ".word " ^ (intListToCSVString l)

    (* Prints the statement *)
    fun prStmt (stm : (string, Reg) Stmt) : string = case stm of
            Inst  i => (prInst i)
            | Dir d => (prDir d)
    
    (* Prints the list of statements *)

end