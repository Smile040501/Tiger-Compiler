signature REG_ALLOC =
sig
    type key   = Temp.value
    type value = Mips.Reg

    val allocRegs       : key list -> unit
    val allocSpecialReg : key -> value -> unit
    val getReg          : key -> value
end

structure RegAlloc :> REG_ALLOC =
struct
    type key   = Temp.value
    type value = Mips.Reg

    exception NoRegistersAvailable of string
    exception NoRegisterForTemp of string

    structure TempValMap = RedBlackMapFn(
        struct
            type ord_key = key
            val compare = Temp.compare
        end
    )

    type mp = value TempValMap.map

    val regList : Mips.Reg list = [Mips.T0, Mips.T1, Mips.T2, Mips.T3, Mips.T4, Mips.T5, Mips.T6, Mips.T7, Mips.T8, Mips.T9, Mips.S0, Mips.S1, Mips.S2, Mips.S3, Mips.S4, Mips.S5, Mips.S6, Mips.S7]

    val curIdx : int ref = ref 0  (* Current index of the list *)

    val MP : mp ref = ref TempValMap.empty
    val MP_Special : mp ref = ref TempValMap.empty

    fun allocReg (k: key) (r: value) (m: mp ref) : unit =
            let
                val newM = TempValMap.insert (!m, k, r)
            in
                m := newM
            end

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

    fun allocSpecialReg (k : key) (r : value) : unit = allocReg k r MP_Special

    fun getReg (k: key) : value = case (TempValMap.find (!MP_Special, k)) of
                  SOME r =>  r
                | NONE   => (case (TempValMap.find (!MP, k)) of
                          SOME r => r
                        | NONE   => raise NoRegisterForTemp ("No register for temp " ^ Temp.prettyValue k)
                )
end
