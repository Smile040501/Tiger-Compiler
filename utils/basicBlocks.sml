signature INST =
sig
    type t   (* The type of the instruction *)
    val isJumpLike : t -> bool
    val isTarget   : t -> bool
end

signature BASIC_BLOCKS =
sig
    structure Inst : INST
    type block
    val basicBlocks : Inst.t list -> block list
end

functor BasicBlocks (I : INST) : BASIC_BLOCKS =
struct

    structure Inst = I  (* Exposing the instruction module as well *)

    type block = I.t list

    fun basicBlocksHelper (insts: I.t list) (comp : block list ref) (curr : block) =
            case (insts, curr) of
                  ([], [])     => ()
                | ([], _)      => (comp := !comp @ [curr])
                | (i :: is, _) =>
                    if (I.isTarget i) andalso (I.isJumpLike i) then
                        (* If instruction is Target and JumpLike, emit the current block, emit the block with the instruction alone and start a new block for the rest of the instructions *)
                        ((comp := !comp @ [curr]);
                            (comp := !comp @ [[i]]);
                            (basicBlocksHelper is comp []))

                    else if (I.isTarget i) then
                        (* If instruction is Target only, emit the current block and start a new block for rest of the instructions with first instruction as i *)
                        ((comp := !comp @ [curr]); (basicBlocksHelper is comp [i]))

                    else if (I.isJumpLike i) then
                        (* If instruction is jumpLike, emit the current block with this instruction as the last instruction and start a new block for the rest of the instructions *)
                        ((comp := !comp @ [curr @ [i]]); (basicBlocksHelper is comp []))

                    else (basicBlocksHelper is comp (curr @ [i]))

    fun basicBlocks (insts : I.t list) =
            let
                val blocks : block list ref = ref []
                val _  = basicBlocksHelper insts blocks []
            in
                !blocks
            end
end
