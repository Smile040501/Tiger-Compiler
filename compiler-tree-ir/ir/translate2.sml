(* Structure to translate Tiger.Prog to Tree.Stm *)
signature TRANSLATE =
sig
    datatype Exp = Ex of Tree.Exp       (* An Expression *)
                |  Nx of Tree.Stm       (* No result *)
                (* Conditional: Pass it a true-destination and a false-destination. It will make a
                statement that evaluates some conditionals and then jump to one of the destinations *)
                |  Cx of Temp.label * Temp.label -> Tree.Stm

    val unEx : Exp -> Tree.Exp
    val unNx : Exp -> Tree.Stm
    val unCx : Exp -> (Temp.label * Temp.label -> Tree.Stm)
end

structure Translate :> TRANSLATE =
struct

    (* structures used *)
    structure TIG = Tiger
    structure T   = Tree
    structure PTA = PrettyTigerAST
    structure RA  = RegAlloc


    (* Utility functions *)
    val updateFirstVal = Utils.updateFirstVal
    val updateLastVal  = Utils.updateLastVal
    val getLastVal     = Utils.getLastVal


    (* Raising exception and printing the error message *)
    exception UnsupportedOperation of string
    exception NotDefined           of string
    exception TranslationError     of string

    fun raiseException (ex: string -> exn) (msg: string) =
                    Utils.throwErr ex ("[translate.sml]:" ^ msg ^ "\n\n")


    (* The result type of intermediate (nested) expressions
       Either it will be an integer result or a resultant register  *)
    datatype Result = IntRes  of int
                    | TempRes of Temp.value

    (* The temporaries allocation performed by the compiler.
       To keep track of all the temporaries (including the nested ones). *)
    (* val temps : (string * Temp.value) list ref *)
    val temps : (string * Temp.value) list ref = ref []

    (* Assign a temporary value to the string if not already there.
       Allocates a new register if new temporary value is created and being mentioned to do so. *)
    (* assignTemp : Env.mp -> string -> bool -> Env.mp * Temp.value *)
    fun assignTemp (env : Env.mp) (id: string) (allocateReg: bool) =
            (case Env.find env id of
                  SOME t => (env, t)
                | NONE   => let
                                val t      = Temp.newValue ()
                                val newEnv = Env.insert env id t
                                (* Storing the (id, t) pair in the list created *)
                                val _      = temps := (!temps @ [(id, t)])
                                (* Allocating register to it if mentioned *)
                                val _      = if allocateReg then (RA.allocReg t)
                                             else ()
                            in
                                (newEnv, t)
                            end
            )

    (* Get the temporary value allocated to the Tiger.Lvalue
       Searches for the variable from the current scope to outer scopes. *)
    (* getTemp : Env.mp list -> string -> Temp.value *)
    fun getTemp (envs : Env.mp list) (id: string) = case envs of
              [] => raiseException NotDefined ("[getTemp]: Undefined variable " ^ id)
            | (e :: es) => (case Env.find e id of
                                  SOME t => t
                                | NONE   => getTemp es id
                            )


    datatype Exp = Ex of Tree.Exp       (* An Expression *)
                |  Nx of Tree.Stm       (* No result *)
                (* Conditional: Pass it a true-destination and a false-destination. It will make a
                statement that evaluates some conditionals and then jump to one of the destinations *)
                |  Cx of Temp.label * Temp.label -> Tree.Stm

    (* val unEx : Exp -> Tree.Exp *)
    fun unEx (Ex e)      = e
      | unEx (Nx s)      = T.ESEQ (T.getEseqRec s (T.CONST 0))
      | unEx (Cx genStm) =
            let
                val r = Temp.newValue()
                val t = Temp.newLabel()
                val f = Temp.newLabel()
            in
                T.ESEQ (T.getEseqRec
                    (T.seq [
                        (* Move 1 into the register *)
                        T.MOVE (T.getMoveRec (T.TEMP r) (T.CONST 1)),
                        (* Conditional jump *)
                        genStm (t, f),
                        (* If condition fails, it enters this block *)
                        T.LABEL f,
                        (* Set register to 0 as condition failed *)
                        T.MOVE (T.getMoveRec (T.TEMP r) (T.CONST 0)),
                        (* If condition succeeds, then register has value 1. *)
                        T.LABEL t
                    ])
                    (T.TEMP r)  (* Result is just the temporary r containing 0 or 1 *)
                )
            end

    (* val unNx : Exp -> Tree.Stm *)
    fun unNx (Ex e)           = T.EXP e
      | unNx (Nx s)           = s
      | unNx (e as Cx genStm) = T.EXP (unEx e)

    (* val unCx : Exp -> (Temp.label * Temp.label -> Tree.Stm) *)
    fun unCx (Ex e)      = (case e of
              T.CONST 1 => (fn (t, f) => T.JUMP  (T.getJumpRec (T.NAME t) [t]))
            | T.CONST 0 => (fn (t, f) => T.JUMP  (T.getJumpRec (T.NAME f) [f]))
            | _         => (fn (t, f) => T.CJUMP (T.getCjumpRec (T.CONST 0) T.EQ e t f))
        )
      | unCx (Nx s)      = Utils.failExit "[unCx]: Not a required conversion\n"
      | unCx (Cx genStm) = genStm

end
