(* Temp structure to maintain temporary variables *)
signature TEMP =
sig
    type temp
    val newtemp    : unit -> temp
    val prettyTemp : temp -> string
end

structure Temp :> TEMP = struct

    type temp  = int (* 2ʷ many variables on a w-sized machine       *)
		             (* We can use `IntInf.int` if we want unbounded *)

    val curTmp : temp ref = ref 0 (* Keep track of how many temps have been allocated *)

    (* Allocates a new Temp.temp *)
    fun newtemp () = let
                        val oldTmp = !curTmp
                     in
                        (curTmp := oldTmp + 1); oldTmp
                     end

    (* Pretty prints `Temp.temp` *)
    fun prettyTemp (t: temp) = Int.toString t
end
