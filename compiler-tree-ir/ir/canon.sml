(* Captures the Canonization of the Tree-IR *)
signature CANON =
sig
    val linearize : Tree.Stm -> Tree.Stm list
end

(*
    Canonical Trees:
    -   No SEQ or ESEQ
    -   The parent of each CALL is either EXP(...) or MOVE(TEMP t, ...)

    Transformations of ESEQ:
    Lift them higher and higher in the tree, until they can become SEQ nodes.

    Canonisation of expressions:
    Takes an expression e as argument and returns a pair of (Stm * Exp) where
    the first component captures the effect of evaluating the expression and
    the second component is the canonical form of the expression.

    Algorithm:
    Canonisation is performed by applying a set of rewrite rules. While
    the rule itself is easy to explain, recursively applying the rewrite
    rules means defining a set of  4 mutually recursive functions.

    val do_stmt      : Tree.Stm -> Tree.Stm
    val do_exp       : Tree.Exp -> Tree.Stm * Tree.Exp
    val reorder_exp  : Tree.Exp list -> eBuilder -> Tree.Stm * Tree.Exp
    val reorder_stmt : Tree.Exp list -> sBuilder -> Tree.Stm

    The result of the above functions is always in canonical form.

    Builders are functions that build a cnonical form of the
    expression/stmt from canonical form of sub-expressions. They
    are just the functions that takes the list of sub-expressions and
    builds the desired expression/stmt back.

    The {[do_*]} function should be seen as the one which extracts the
    sub-expressions (that need canonising) and supplies it to the
    {[reoder_*]} functions together with the associated builder.

    The {[reoder_*]} functions in-turn calls the builder on the cannonical
    form of its subexpressions. The canonical form is got by calling the
    ``reorder`` function which will recursively call the {[do_*]} functions.

    (* val reorder : Tree.Exp list -> (Tree.Stm, Tree.Exp list) *)
    -   (Tree.CALL :: es) => For the case there are multiple call statements,
            move its result into a new temporary, so that consecutive calls
            don't overwrite each other's results.
    -   (e :: es) => let val (cs, ce) = do_exp e
                        val (css, ces) = reorder es
        Else just check whether the the css and the ce commute, and in case they do,
        then directly combine the (cs, css) , otherwise, we'll have to first put
        ce into a temporary register before executing css, to maintain the order.

*)

(* Structure to do the Tree-IR canonization *)
structure Canon :> CANON =
struct
    (* Structures used *)
    structure T = Tree


    (* Raising exception and printing the error message *)
    exception BuildError of string

    fun raiseException (ex: string -> exn) (msg: string) =
                    Utils.throwErr ex ("[canon.sml]:" ^ msg ^ "\n\n")


    (* Types of the builder functions *)
    type eBuilder = T.Exp list -> T.Exp
    type sBuilder = T.Exp list -> T.Stm

    (* Helper function for builder functions *)
    fun buildError msg = raiseException BuildError msg

    (* Builder functions for statements/expressions *)
    fun b_BINOP (oper : T.Binop) : eBuilder =
            fn [cleft, cright] => T.BINOP (T.getBinopRec cleft oper cright)
             | _               => buildError "BINOP"

    val b_MEM : eBuilder =
            fn [ce] => T.MEM ce
             | _    => buildError "MEM"

    val b_CALL : eBuilder =
            fn (cfunc :: cargs) => T.CALL (T.getCallRec cfunc cargs)
             | _                => buildError "CALL"

    val b_MOVE : sBuilder =
            fn [clhs, crhs] => T.MOVE (T.getMoveRec clhs crhs)
             | _            => buildError "MOVE"

    val b_EXP : sBuilder =
            fn [ce] => T.EXP ce
             | _    => buildError "EXP"

    fun b_JUMP (labs : Temp.label list) : sBuilder =
            fn [caddr] => T.JUMP (T.getJumpRec caddr labs)
             | _       => buildError "JUMP"

    fun b_CJUMP (oper : T.Relop) (tLab : Temp.label) (fLab : Temp.label) : sBuilder =
            fn [cleft, cright] => T.CJUMP (T.getCjumpRec cleft oper cright tLab fLab)
             | _               => buildError "CJUMP"


    (* Nop operation *)
    val nop = T.EXP (T.CONST 0)

    (* Reorders the list of expressions *)
    (* val reorder : Tree.Exp list -> (Tree.Stm, Tree.Exp list) *)
    fun reorder []                            = (nop, [])

    (* For the case there are multiple call statements,
        move its result into a new temporary, so that
        consecutive calls don't overwrite each other's results. *)
    (* This case should ideally never happen as we put all CALL expressions as child of EXP
        while translating the Tiger AST to Tree IR *)
      | reorder ((T.CALL {func, args}) :: es) =
            let
                val nt = T.newSpecialTemp ()    (* New temporary register *)
                (* Move result into a new temporary *)
                val ns = T.ESEQ (T.getEseqRec (
                        T.MOVE (T.getMoveRec nt (T.CALL (T.getCallRec func args)))
                    ) nt)
            in
                reorder (ns :: es)
            end

    (* Else just check whether the the css and the ce commute, and in case they do,
        then directly combine the (cs, css) , otherwise, we'll have to first put
        ce into a temporary register before executing css, to maintain the order. *)
      | reorder (e :: es)                     =
            let
                val (cs,  ce)  = do_exp e    (* Canonical form of the expression *)
                val (css, ces) = reorder es  (* Canonical form of the rest of the expressions *)

                (* Estimates (very naively) whether two expressions commute *)
                (* val does_commute : Tree.Stm -> Tree.Exp -> bool *)
                fun does_commute (T.EXP (T.CONST _)) _           = true
                  | does_commute _                   (T.NAME _)  = true
                  | does_commute _                   (T.CONST _) = true
                  | does_commute _                   _           = false
            in
                case (does_commute css ce) of
                      true  => (T.seq [cs, css], ce :: ces)
                    | false =>  let
                                    val nt = T.newSpecialTemp ()  (* New temporary register *)
                                in
                                    (* Move result into a temporary to maintain the order *)
                                    (T.seq [cs, T.MOVE (T.getMoveRec nt ce), css], nt :: ces)
                                end
            end

    (* Reorders expressions and applies the builder function *)
    (* val reorder_exp  : Tree.Exp list -> eBuilder -> Tree.Stm * Tree.Exp *)
    and reorder_exp (es : T.Exp list) (bfn : eBuilder) : (T.Stm * T.Exp) =
            let
                val (cs, ces) = reorder es
            in
                (cs, bfn ces)
            end

    (* Reorders expressions and applies the builder function *)
    (* val reorder_stmt : Tree.Exp list -> sBuilder -> Tree.Stm *)
    and reorder_stmt (es : T.Exp list) (bfn : sBuilder) : T.Stm =
            let
                val (cs, ces) = reorder es
            in
                T.seq [cs, bfn ces]
            end

    (* Extracting all the expressions out of it, so that we can reorder them and then put is back into place *)
    (* val do_exp : Tree.Exp -> Tree.Stm * Tree.Exp *)
    and do_exp (T.BINOP {left, oper, right}) = reorder_exp [left, right] (b_BINOP oper)
      | do_exp (T.MEM e)                     = reorder_exp [e]            b_MEM
      (* This case should ideally never happen as we put all CALL expressions as child of EXP
        while translating the Tiger AST to Tree IR *)
      | do_exp (T.CALL {func, args})         = reorder_exp (func :: args) b_CALL
      (* This case should ideally never happen as we extracted out the statement from
        it and we are always using one single `resultTemp` register to store its result
        while translating the Tiger AST to Tree IR *)
      | do_exp (T.ESEQ {stm, res})           =
            let
                val cs        = do_stmt stm  (* Canonicalized Statement  *)
                val (cse, ce) = do_exp res   (* Canonicalized Expression *)
            in
                (T.seq [cs, cse], ce)
            end
      | do_exp e                             = (nop, e)

    (* Extracting all the expressions out of it, so that we can reorder them and then put is back into place *)
    (* val do_stmt : Tree.Stm -> Tree.Stm *)
    and do_stmt (T.MOVE {lhs, rhs})                       =
            (case (lhs, rhs) of
            (* Handling special cases when moving to `T.TEMP` or `T.MEM`,
                as we can directly move to that location without storing the result
                in another register first... *)
            (* This case should ideally never happen as we put all CALL expressions as child of EXP
                while translating the Tiger AST to Tree IR *)
                (T.TEMP t, T.CALL {func, args}) => (reorder_stmt (func :: args) (
                                                        fn l => b_MOVE [T.TEMP t, b_CALL l]
                                                    ))
              | (T.TEMP t, e)                   => (reorder_stmt [e] (
                                                        fn [ce] => b_MOVE [T.TEMP t, ce]
                                                         | _    => buildError "MOVE"
                                                    ))
              | (T.MEM e1, e2)                  => (reorder_stmt [e1, e2] (
                                                        fn [ce1, ce2] => b_MOVE [T.MEM ce1, ce2]
                                                         | _          => buildError "MOVE"
                                                    ))
              | (e1, e2)                        => reorder_stmt [e1, e2] b_MOVE
            )
      | do_stmt (T.EXP e)                                 =
            (case e of
            (* Since, call is inside EXP, we don't need its value and
                so there is no need to move it into a register. By doing this,
                we are hiding `CALL` statements from the ``reorder`` function *)
                (T.CALL {func, args}) => (reorder_stmt (func :: args) (
                                            fn l => b_EXP [b_CALL l]
                                        ))
              | e                     => reorder_stmt [e] b_EXP
            )
      | do_stmt (T.JUMP {addr, labs})                     = reorder_stmt [addr] (b_JUMP labs)
      | do_stmt (T.CJUMP {left, oper, right, tLab, fLab}) = reorder_stmt [left, right] (b_CJUMP oper tLab fLab)
      | do_stmt (T.SEQ {s1, s2})                          = T.seq [do_stmt s1, do_stmt s2]
      | do_stmt (T.LABEL lab)                             = T.LABEL lab


    (* At the end, we will only have SEQ statements left.
        This function makes a list by removing the SEQ constructs.
        Linearised expression might look like:
            SEQ (a, SEQ (b, ....)), this function converts
            it to a list which looks like [a, b, ....] *)
    (* val linear : Tree.Stm -> Tree.Stm list -> Tree.Stm list *)
    fun linear (T.SEQ {s1, s2}) es = linear s1 (linear s2 es)
      | linear s                es = s :: es

    (* Remove `nop`s from the statements *)
    (* val remNops : Tree.Stm list -> Tree.Stm list *)
    fun remNops []                          es = es
      | remNops ((T.EXP (T.CONST 0)) :: ss) es = remNops ss es
      | remNops (s :: ss)                   es = remNops ss (es @ [s])

    (* Linearize the statement *)
    (* val linearize : Tree.Stm -> Tree.Stm list *)
    fun linearize stm = remNops (linear (do_stmt stm) []) []

end
