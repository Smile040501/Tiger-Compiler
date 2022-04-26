signature FRAME = sig
    type Offset
    type Frame

    val wordSize    : Offset
    val getWordSize : int

    val stackAllocStmt   : int -> Tree.Stm
    val allocVar         : Tiger.id -> Frame -> Tiger.id -> Frame
    val allocInternalVar : Frame -> Frame * int

    val emptyFrame   : Frame
    val getVarOffset : Frame list -> Tiger.id -> (Tiger.id * int) option

    val setOffset : Frame -> Frame -> Frame
    val setMap    : Frame -> Frame -> Frame

    val getFramePointer : Tiger.id -> Tiger.id -> Tree.Stm
end

structure Frame :> FRAME = struct

    exception Unimplemented of string

    type Offset = int

    structure IDMap = RedBlackMapFn (Tiger.IDKey)

    (* The map in frame outputs the name of the function and
        the offset given the name of a variable.
        The offset will be the actual offset with sign. *)
    datatype Frame = Frame of Offset * ((Tiger.id * Offset) IDMap.map)

    (* WordSize of the machine *)
    val wordSize    : Offset = 4
    val getWordSize : int    = wordSize

    (* Constructs a stack allocation statement
        given the number of variables allocated on the stack *)
    (* val stackAllocStmt : int -> Tree.Stm *)
    fun stackAllocStmt numAllocs = Tree.seq [
        Tree.MOVE (Tree.getMoveRec Tree.argTemp1 (Tree.CONST (~wordSize * numAllocs))),
        Tree.MOVE (Tree.getMoveRec Tree.stackTemp
                        (Tree.BINOP (Tree.getBinopRec Tree.stackTemp Tree.PLUS Tree.argTemp1))
                    )
    ]

    (* Allocates a variable on the stack *)
    (* val allocVar : Tiger.id -> Frame -> Tiger.id -> Frame *)
    fun allocVar (currentFunc: Tiger.id) (Frame (currOffset, varMap)) (var: Tiger.id) =
        let
            val newMap = case IDMap.find (varMap, var) of
                    (* If variable is not present on stack, then allocate it *)
                    NONE => IDMap.insert (varMap, var, (currentFunc, currOffset))

                    (* If variable is already present on the stack, then update the map *)
                    | SOME (prevFuncName, prevOffset) =>
                        let
                            val (mapAfterRemove, _) = IDMap.remove (varMap, var)
                        in
                            IDMap.insert (mapAfterRemove, var, (currentFunc, currOffset))
                        end

            (* New offset after variable has been allocated on the stack *)
            val newOffset = currOffset - wordSize
        in
            Frame (newOffset, newMap)
        end

    (* Allocates an internal variable on the stack *)
    (* val allocInternalVar : Frame -> Frame * int *)
    fun allocInternalVar (Frame (currOffset, varMap)) =
                                    (Frame (currOffset - wordSize, varMap), currOffset)

    (* Constructs an empty frame *)
    (* val emptyFrame : Frame *)
    val emptyFrame = Frame (0, IDMap.empty)

    (* Returns the offset of a variable on the stack *)
    (* val getVarOffset : Frame list -> Tiger.id -> (Tiger.id * int) option *)
    fun getVarOffset []                          _   = NONE
      | getVarOffset ((Frame (_, varMap)) :: fs) var =
            let
                val res = IDMap.find (varMap, var)
            in
                case res of
                        NONE   => getVarOffset fs var
                    |   SOME _ => res
            end

    (* Sets the offset of Frame2 to Frame1 and returns the updated Frame *)
    (* val setOffset : Frame -> Frame -> Frame *)
    fun setOffset (Frame (offset1, varMap1)) (Frame (offset2, varMap2)) = Frame (offset2, varMap1)

    (* Sets the map of Frame2 to Frame1 and returns the updated Frame *)
    (* val setMap : Frame -> Frame -> Frame *)
    fun setMap (Frame (offset1, varMap1)) (Frame (offset2, varMap2)) = Frame (offset1, varMap2)

    (* Place the frame pointer value of the function into the `resultTemp`
        Currently, we only have one function "main" and hence `currFunc = ancFunc` *)
    (* val getFramePointer : Tiger.id -> Tiger.id -> Tree.Stm *)
    fun getFramePointer currFunc ancFunc = Tree.MOVE (Tree.getMoveRec Tree.resultTemp Tree.frameTemp)
end
