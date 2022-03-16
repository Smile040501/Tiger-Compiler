(* Register allocation of temporary registers with actual MIPS registers *)
(* Currently it is implemented as GREEDY ALLOCATION of registers *)
signature REG_ALLOC =
sig
    type key   = Temp.value     (* Type of temporary registers *)
    type value = Mips.Reg       (* Type of MIPS registers *)

    (* Allocates the input register to a temporary register *)
    val allocRegWith  : key -> value -> unit

    (* Allocates a new register to a temporary register *)
    val allocReg  : key -> unit

    (* Allocates registers to list of temporary registers *)
    val allocRegs : key list -> unit

    (* Returns the MIPS register allocated to the temporary register *)
    val getReg    : key -> value

    (* Returns the register allocation performed by the compiler *)
    val listItems : unit -> (string * string) list
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

    (* Map from Temp.value to Mips.Reg *)
    val MP : mp ref = ref TempValMap.empty

    (* Allocates the input register to a temporary register *)
    (* allocRegWith : key -> value -> unit *)
    fun allocRegWith (k: key) (r: value) : unit =
                let
                    val newM = TempValMap.insert (!MP, k, r)
                in
                    MP := newM
                end

    (* Allocates a new register to a temporary register *)
    (* allocReg  : key -> unit *)
    fun allocReg (k: key) : unit =
                if !curIdx >= (List.length regList) then
                    (Utils.throwErr NoRegistersAvailable
                        "[regAlloc.sml]:[allocRegs]: No more registers available")
                else
                    let
                        val r = List.nth (regList, !curIdx)
                        val _ = curIdx := !curIdx + 1
                    in
                        allocRegWith k r
                    end

    (* Allocate registers to list of temporary registers *)
    (* allocRegs : key list -> unit *)
    fun allocRegs ([] : key list) : unit = ()
      | allocRegs (k :: ks)              = ((allocReg k); allocRegs ks)

    (* Returns the MIPS register allocated to the temporary register *)
    (* getReg : key -> value *)
    fun getReg (k: key) : value = case (TempValMap.find (!MP, k)) of
                      SOME r => r
                    | NONE   => (Utils.throwErr NoRegisterForTemp
                                    ("[regAlloc.sml]:[getReg]: No register is allocated for temp " ^
                                        Temp.prettyValue k)
                                )

    (* Returns the register allocation performed by the compiler *)
    (* listItems: unit -> (string * string) list *)
    fun listItems () = (map (fn (k, v) => (Temp.prettyValue k, PrettyMips.prettyReg v))
                                                                (TempValMap.listItemsi (!MP))
                        )
end
