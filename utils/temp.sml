(* Temp structure to maintain temporary variables *)
signature TEMP =
sig
    type value
    type label = string

    val newtemp     : unit  -> value
    val newLabel    : unit  -> label

    val compare     : value -> value -> bool

    val prettyTemp  : value -> string
    val prettyLabel : label -> string
end

structure Temp :> TEMP = struct

    type value  = int (* 2ʷ many variables on a w-sized machine       *)
		             (* We can use `IntInf.int` if we want unbounded *)

    type label  = string

    val curTmp   : value ref = ref 0 (* Keeps track of how many temps have been allocated *)
    val curLabel : int ref   = ref 0 (* Keeps track of how many strings have been allocated *)

    (* Allocates a new `Temp.value` *)
    fun newtemp () = let
                        val oldTmp = !curTmp
                     in
                        (curTmp := oldTmp + 1); oldTmp
                     end

    (* Allocates a new `Temp.label` *)
    fun newLabel () = let
                        val oldLabel = !curLabel
                      in
                        (curLabel := oldLabel + 1); "TL" ^ (Int.toString oldLabel)
                      end

    (* Compare two values *)
    fun compare v1 v2 = (v1 = v2)

    (* Pretty prints `Temp.value` *)
    fun prettyTemp (t: value) = Int.toString t

    (* Pretty prints `Temp.label` *)
    fun prettyLabel (t: label) = t
end
