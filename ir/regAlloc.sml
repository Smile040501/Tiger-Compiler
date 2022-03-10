(* Register allocation of temporary registers with actual MIPS registers *)
(* Currently it is implemented as GREEDY ALLOCATION of registers *)
signature REG_ALLOC =
sig
    type key   = Temp.value     (* Type of temporary registers *)
    type value = Mips.Reg       (* Type of MIPS registers *)

    (* Allocate registers to list of temporary registers *)
    val allocRegs       : key list -> unit

    (* Allocate some special registers directly *)
    val allocSpecialReg : key -> value -> unit

    (* Returns the MIPS register allocated to the temporary register *)
    val getReg          : key -> value
end

structure RegAlloc :> REG_ALLOC =
struct
    type key   = Temp.value
    type value = Mips.Reg

    (* Useful Exceptions *)
    exception NoRegistersAvailable of string
    exception NoRegisterForTemp of string

    (* Map for having Temp.value as the key *)
    structure TempValMap = RedBlackMapFn(
        struct
            type ord_key = key
            val compare = Temp.compare
        end
    )

    type mp = value TempValMap.map

    (* MIPS registers that can be used for greedy register allocation *)
    val regList : Mips.Reg list = [Mips.T0, Mips.T1, Mips.T2, Mips.T3, Mips.T4, Mips.T5, Mips.T6, Mips.T7, Mips.T8, Mips.T9, Mips.S0, Mips.S1, Mips.S2, Mips.S3, Mips.S4, Mips.S5, Mips.S6, Mips.S7]

    val curIdx : int ref = ref 0  (* Current index of the list *)

    (* Map for Temp.values and some special registers (like A0 and V0) *)
    val MP : mp ref = ref TempValMap.empty
    val MP_Special : mp ref = ref TempValMap.empty

    (* Insert the key-value pair in the iput map *)
    (* allocReg : key -> value -> mp ref -> unit *)
    fun allocReg (k: key) (r: value) (m: mp ref) : unit =
            let
                val newM = TempValMap.insert (!m, k, r)
            in
                m := newM
            end

    (* Allocate registers to list of temporary registers *)
    (* allocRegs : key list -> unit *)
    fun allocRegs ([] : key list) : unit = ()
      | allocRegs (k :: ks) =
            if !curIdx >= (List.length regList) then
                raise NoRegistersAvailable "No more registers available"
            else
                let
                    val r = List.nth (regList, !curIdx)
                in
                    (curIdx := !curIdx + 1); (allocReg k r MP); allocRegs ks
                end

    (* Allocate some special registers directly *)
    (* allocSpecialReg : key -> value -> unit *)
    fun allocSpecialReg (k : key) (r : value) : unit = allocReg k r MP_Special

    (* Returns the MIPS register allocated to the temporary register *)
    (* getReg : key -> value *)
    fun getReg (k: key) : value = case (TempValMap.find (!MP_Special, k)) of
                  SOME r =>  r
                | NONE   => (case (TempValMap.find (!MP, k)) of
                          SOME r => r
                        | NONE   => raise NoRegisterForTemp ("No register for temp " ^ Temp.prettyValue k)
                )
end
