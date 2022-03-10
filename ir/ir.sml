(* Structure for intermediate representation *)
signature IR =
sig
    type Inst = (string, Temp.value) Mips.Instruction
    type Stmt = (string, Temp.value) Mips.Stmt
    type Prog = Stmt list

    val prettyInst : Inst -> string
    val prettyStmt : Stmt -> string
    (* val prettyProg : Prog -> string *)
end

structure Ir :> IR =
struct
    type Inst = (string, Temp.value) Mips.Instruction
    type Stmt = (string, Temp.value) Mips.Stmt
    type Prog = Stmt list

    fun identity x = x  (* Identity function *)

    (* Functions to pretty print IR *)
    fun prettyInst i = PrettyMips.prettyMapInst identity Temp.prettyValue i

    fun prettyStmt s = PrettyMips.prettyMapStmt identity Temp.prettyValue s
end
