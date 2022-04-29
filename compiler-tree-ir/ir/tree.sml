structure Tree :> TREE =
struct
   (* Computation of some value (possibly with side-effects) *)
   datatype Exp =
               (* The integer constant *)
                  CONST of int

               (* Assembly language label   *)
               |  NAME  of Temp.label

               (* Assebly language register *)
               |  TEMP  of Temp.value

               (* BINOP(o, e1, e2): The application of binary operation o to operands e1 and e2
                  e1 is evaulated before e2 *)
               |  BINOP of {left: Exp, oper: Binop, right: Exp}

               (* Contents of `wordSize` bytes of memory starting at address Exp
                  `wordSize` is defined in the FRAME module *)
               (* When it is used as left child of MOVE, it means "store",
                  but anywhere else it means "fetch" *)
               |  MEM   of Exp

               (* CALL(f, l): A procedure call, the application of function f to the argument list l
                  The subexpression f is evaluated first before the arguments
                  which are evaluated from left to right *)
               |  CALL  of {func: Exp, args: Exp list}

               (* ESEQ(s, e): The statement s is evaluated for side effects,
                  then e is evaluated for a result *)
               |  ESEQ  of {stm: Stm, res: Exp}

   (* Perform side-effects and control flow *)
   and Stm     =
               (* MOVE(TEMP t, e): Evaluate e and move it into temporary t *)
               (* MOVE(MEM(e1), e2): Evaluate e1, yielding address a. Then evaluate e2, and store the
                  results into wordSize bytes of memory starting at a *)
                  MOVE  of {lhs: Exp, rhs: Exp}

               (* EXP(e): Evaluate e and discard the result *)
               |  EXP   of Exp

               (* JUMP(e, labs): Transfer control (jump) to address e
                  The destination e may be a literal label, as in NAME(lab),
                  or it may be an address calculated by any other kind of expression *)
               (* The list of labels `labs` specifies all the possible locations that
                  the expression e can evaluate to. *)
               (* Ex: Jumping to a known lable l → JUMP(NAME l, [l]) *)
               |  JUMP  of {addr: Exp, labs: Temp.label list}

               (* CJUMP(o, e1, e2, t, f): Evaluate e1, e2 in that order, yielding value `a` and `b`.
                  Then compare a, b using the relation operator o.
                  If the result is true, jump to `t`, else jump to `f` *)
               |  CJUMP of {left: Exp, oper: Relop, right: Exp, tLab: Temp.label, fLab: Temp.label}

               (* The statement s1 followed by s2 *)
               |  SEQ   of {s1: Stm, s2: Stm}

               (* LABEL(n): Define the constant value of name n to the current machine code address
                  Like a label definition in assembly language.
                  NAME(n) may be the target of jumps, calls, etc. *)
               |  LABEL of Temp.label

   (* The integer logical shift operators are LSHIFT and RSHIFT *)
   (* The integer arithmetic right-shift is ARSHIFT *)
   and Binop   =  PLUS | MINUS | MUL | DIV
               |  AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

   and Relop   =  EQ | NE | LT | GT | LE | GE
               |  ULT | ULE | UGT | UGE

   (* Already allocated special temporary values *)
   val resultTemp     = TEMP Temp.resultTemp
   val argTemp1       = TEMP Temp.argTemp1
   val argTemp2       = TEMP Temp.argTemp2
   val frameTemp      = TEMP Temp.framePointer
   val stackTemp      = TEMP Temp.stackPointer
   val returnTemp     = TEMP Temp.returnValue
   val returnAddrTemp = TEMP Temp.returnAddr

   (* Allocates a new special temporary register *)
   (* val newSpecialTemp : unit -> Exp *)
   fun newSpecialTemp () = TEMP (Temp.newSpecialValue ())

   exception EmptySeq of string

   (* Utility functions *)
   fun getBinopRec left oper right = {left = left, oper = oper, right = right}
   fun getCallRec  func args       = {func = func, args = args}
   fun getEseqRec  stm  res        = {stm = stm, res = res}
   fun getMoveRec  lhs  rhs        = {lhs = lhs, rhs = rhs}
   fun getJumpRec  addr labs       = {addr = addr, labs = labs}

   fun getCjumpRec left oper right tLab fLab =
                                 {left = left, oper = oper, right = right, tLab = tLab, fLab = fLab}

   fun getSeqRec   s1   s2         = {s1 = s1, s2 = s2}

   (* This function shortens the SEQ expression by removing unnecessary sub-expressions *)
   (* val shorten_seq : Stm -> Stm -> Stm *)
   fun shorten_seq (EXP(CONST _)) s2             = s2
     | shorten_seq s1             (EXP(CONST _)) = s1
     | shorten_seq s1             s2             = SEQ (getSeqRec s1 s2)

   (* val seq : Stm list -> Stm *)
   (* Converts a sequence of statements list to a single Tree.Stm *)
   fun  seq [x]       = x
      | seq (x :: xs) = shorten_seq x (seq xs)
      | seq _         = Utils.throwErr EmptySeq ("[seq]: Empty list")

   (* val moveTempToFrame : int -> Exp -> Stm *)
   (* Moves the value of a temporary to the frame at offset given *)
   fun moveTempToFrame var_offset temp =
         MOVE (getMoveRec (MEM (BINOP (getBinopRec frameTemp PLUS (CONST var_offset)))) temp)

   (* val moveFrameToTemp : Exp -> int -> Stm *)
   (* Moves the value from the frame at the offset given to a temporary *)
   fun moveFrameToTemp temp var_offset =
         MOVE (getMoveRec temp (MEM (BINOP (getBinopRec frameTemp PLUS (CONST var_offset)))))
end
