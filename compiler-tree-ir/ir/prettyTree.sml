(* Pretty Prints the Tree IR *)
signature PRETTY_TREE =
sig
    val prettyTreeExp   : Tree.Exp   -> string
    val prettyTreeStm   : Tree.Stm   -> string
    val prettyTreeBinop : Tree.Binop -> string
    val prettyTreeRelop : Tree.Relop -> string
end

(* Structure to pretty print the Tree IR code *)
structure PrettyTree :> PRETTY_TREE =
struct
    open Tree

    val indent = Utils.indent   (* Indentation function *)

    (* Converts Exp datatype to string *)
    (* is : indentation that has to be applied on the first line string of the stringified form *)
    (* il : indentation that should be actually be applied on the first line string ideally *)
    (* val strExp : Tree.Exp -> int -> int -> string *)
    fun strExp (CONST i)                   is _  = indent ("CONST(" ^ (Int.toString i)      ^ ")") is
      | strExp (NAME  tl)                  is _  = indent ("NAME("  ^ (Temp.prettyLabel tl) ^ ")") is
      | strExp (TEMP  tv)                  is _  = indent ("TEMP("  ^ (Temp.prettyValue tv) ^ ")") is
      | strExp (BINOP {left, oper, right}) is il =
                let
                    val strLeft  = strExp   left  0 (il + 12)   (* il + 4 + cnt('left  = ') *)
                    val strOper  = strBinop oper
                    val strRight = strExp   right 0 (il + 12)   (* il + 4 + cnt('right = ') *)
                in
                    (indent "BINOP({\n" is) ^
                    (indent ("left  = " ^ strLeft  ^ ",\n") (il + 4)) ^
                    (indent ("oper  = " ^ strOper  ^ ",\n") (il + 4)) ^
                    (indent ("right = " ^ strRight ^ "\n")  (il + 4)) ^
                    (indent "})" il)
                end
      | strExp (MEM e)                     is il = indent ("MEM(" ^ (strExp e 0 (il + 5)) ^ ")") is
      | strExp (CALL {func, args})         is il =
                let
                    val strFunc = strExp  func 0 (il + 11)    (* il + 4 + cnt('func = ') *)
                in
                    (indent "CALL({\n" is) ^
                    (indent ("func = " ^ strFunc ^ ",\n") (il + 4)) ^
                    (case args of
                          [] => (indent "args = []\n" (il + 4))
                        | _  => ((indent "args = [\n" (il + 4)) ^
                                 (strExps args (il + 8) (il + 8)) ^
                                 (indent "]\n" (il + 4))
                                )
                    ) ^
                    (indent "})" il)
                end
      | strExp (ESEQ {stm, res})           is il =
                let
                    val strStmt = strStm stm 0 (il + 10)    (* il + 4 + cnt('stm = ') *)
                    val strRes  = strExp res 0 (il + 10)    (* il + 4 + cnt('res = ') *)
                in
                    (indent "ESEQ({\n" is) ^
                    (indent ("stm = " ^ strStmt ^ ",\n") (il + 4)) ^
                    (indent ("res = " ^ strRes  ^ "\n")  (il + 4)) ^
                    (indent "})" il)
                end

    (* Converts the list of Exp datatype to string *)
    (* val strExps : Tree.Exp list -> int -> int -> string *)
    and strExps []        _  _  = ""
      | strExps [x]       is il = (strExp x is il) ^ "\n"
      | strExps (x :: xs) is il = (strExp x is il) ^ ",\n" ^ (strExps xs is il)

    (* Converts Stm datatype to string *)
    (* val strStm : Tree.Stm -> int -> int -> string *)
    and strStm (MOVE {lhs, rhs})                       is il =
                let
                    val strLHS = strExp lhs 0 (il + 10)    (* il + 4 + cnt('lhs = ') *)
                    val strRHS = strExp rhs 0 (il + 10)    (* il + 4 + cnt('rhs = ') *)
                in
                    (indent "MOVE({\n" is) ^
                    (indent ("lhs = " ^ strLHS ^ ",\n") (il + 4)) ^
                    (indent ("rhs = " ^ strRHS ^ "\n")  (il + 4)) ^
                    (indent "})" il)
                end
      | strStm (EXP e)                                 is il =
                let
                    val strE = strExp e 0 (il + 5)
                in
                    indent ("EXP(" ^ strE ^ ")") is
                end
      | strStm (JUMP {addr, labs})                     is il =
                let
                    val strAddr = strExp    addr 0 (il + 11)    (* il + 4 + cnt('addr = ') *)
                    val strLabs = strLabels labs
                in
                    (indent "JUMP({\n" is) ^
                    (indent ("addr = "  ^ strAddr ^ ",\n") (il + 4)) ^
                    (indent ("labs = [" ^ strLabs ^ "]\n") (il + 4)) ^
                    (indent "})" il)
                end
      | strStm (CJUMP {left, oper, right, tLab, fLab}) is il =
                let
                    val strLeft  = strExp   left  0 (il + 12)    (* il + 4 + cnt('left  = ') *)
                    val strOper  = strRelop oper
                    val strRight = strExp   right 0 (il + 12)    (* il + 4 + cnt('right = ') *)
                    val strTLab  = Temp.prettyLabel tLab
                    val strFLab  = Temp.prettyLabel fLab
                in
                    (indent "CJUMP({\n" is) ^
                    (indent ("left  = " ^ strLeft  ^ ",\n") (il + 4)) ^
                    (indent ("oper  = " ^ strOper  ^ ",\n") (il + 4)) ^
                    (indent ("right = " ^ strRight ^ ",\n") (il + 4)) ^
                    (indent ("tLab  = " ^ strTLab  ^ ",\n") (il + 4)) ^
                    (indent ("fLab  = " ^ strFLab  ^ "\n")  (il + 4)) ^
                    (indent "})" il)
                end
      | strStm (SEQ {s1, s2})                          is il =
                let
                    val strS1 = strStm s1 0 (il + 9)    (* il + 4 + cnt('s1 = ') *)
                    val strS2 = strStm s2 0 (il + 9)    (* il + 4 + cnt('s2 = ') *)
                in
                    (indent "SEQ({\n" is) ^
                    (indent ("s1 = " ^ strS1 ^ ",\n") (il + 4)) ^
                    (indent ("s2 = " ^ strS2 ^ "\n")  (il + 4)) ^
                    (indent "})" il)
                end
      | strStm (LABEL tl) is il = indent ("LABEL(" ^ (Temp.prettyLabel tl) ^ ")") is

    (* Converts the list of Temp.Labels datatype to string *)
    (* val strLabels : Temp.Label list -> string *)
    and strLabels []        = ""
      | strLabels [x]       = (Temp.prettyLabel x)
      | strLabels (x :: xs) = (Temp.prettyLabel x) ^ ", " ^ (strLabels xs)

    (* Converts Binop datatype to string *)
    (* val strBinop : Tree.Binop -> string *)
    and strBinop PLUS    = "PLUS"
      | strBinop MINUS   = "MINUS"
      | strBinop MUL     = "MUL"
      | strBinop DIV     = "DIV"
      | strBinop AND     = "AND"
      | strBinop OR      = "OR"
      | strBinop LSHIFT  = "LSHIFT"
      | strBinop RSHIFT  = "RSHIFT"
      | strBinop ARSHIFT = "ARSHIFT"
      | strBinop XOR     = "XOR"

    (* Converts Relop datatype to string *)
    (* val strRelop : Tree.Relop -> string *)
    and strRelop EQ  = "EQ"
      | strRelop NE  = "NE"
      | strRelop LT  = "LT"
      | strRelop GT  = "GT"
      | strRelop LE  = "LE"
      | strRelop GE  = "GE"
      | strRelop ULT = "ULT"
      | strRelop ULE = "ULE"
      | strRelop UGT = "UGT"
      | strRelop UGE = "UGE"

    (* Returns the string representation of Tree.Exp *)
    (* val prettyTreeExp : Tree.Exp   -> string *)
    fun prettyTreeExp e = (strExp e 0 0) ^ "\n"

    (* Returns the string representation of Tree.Stm *)
    (* val prettyTreeStm : Tree.Stm   -> string *)
    fun prettyTreeStm s = (strStm s 0 0) ^ "\n"

    (* Returns the string representation of Tree.Binop *)
    (* val prettyTreeBinop : Tree.Binop -> string *)
    fun prettyTreeBinop oper = (strBinop oper) ^ "\n"

    (* Returns the string representation of Tree.Relop *)
    (* val prettyTreeRelop : Tree.Relop -> string *)
    fun prettyTreeRelop oper = (strRelop oper) ^ "\n"
end
