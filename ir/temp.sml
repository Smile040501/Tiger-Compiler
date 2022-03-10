(* Temp structure to maintain temporary variables *)
signature TEMP =
sig
    type value
    val newtemp    : unit -> value
    val prettyTemp : value -> string
end

structure Temp :> TEMP = struct

    type value  = int (* 2ʷ many variables on a w-sized machine       *)
		             (* We can use `IntInf.int` if we want unbounded *)

    val curTmp : value ref = ref 0 (* Keep track of how many temps have been allocated *)

    (* Allocates a new Temp.value *)
    fun newtemp () = let
                        val oldTmp = !curTmp
                     in
                        (curTmp := oldTmp + 1); oldTmp
                     end

    (* Pretty prints `Temp.value` *)
    fun prettyTemp (t: value) = Int.toString t
end
