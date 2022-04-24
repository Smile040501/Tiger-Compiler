signature TREE =
sig
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
   val argTemp1       : Exp
   val argTemp2       : Exp
   val resultTemp     : Exp
   val frameTemp      : Exp
   val stackTemp      : Exp
   val returnTemp     : Exp
   val returnAddrTemp : Exp

   (* Utilitiy functions *)
   val getBinopRec : Exp -> Binop -> Exp -> {left: Exp, oper: Binop, right: Exp}
   val getCallRec  : Exp -> Exp list -> {func: Exp, args: Exp list}
   val getEseqRec  : Stm -> Exp -> {stm: Stm, res: Exp}
   val getMoveRec  : Exp -> Exp -> {lhs: Exp, rhs: Exp}
   val getJumpRec  : Exp -> Temp.label list -> {addr: Exp, labs: Temp.label list}
   val getCjumpRec : Exp -> Relop -> Exp -> Temp.label -> Temp.label ->
                              {left: Exp, oper: Relop, right: Exp, tLab: Temp.label, fLab: Temp.label}
   val getSeqRec   : Stm -> Stm -> {s1: Stm, s2: Stm}
   val seq         : Stm list -> Stm

   val moveTempToFrame : int -> Exp -> Stm
   val moveFrameToTemp : Exp -> int -> Stm
end
