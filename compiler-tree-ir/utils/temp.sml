(* Temp structure to maintain temporary variables *)
signature TEMP =
sig
    type value            (* Temporary values (int here) *)
    type label = string   (* Temporary strings (stringified integers) *)

    val DUMMY_VALUE : value

    (* Already allocated special temporary values *)
    val argTemp1     : value
    val argTemp2     : value
    val resultTemp   : value
    val framePointer : value
    val stackPointer : value
    val returnAddr   : value
    val returnValue  : value

    val newValue    : unit   -> value
    val newLabel    : unit   -> label
    val strToLabel  : string -> label

    val areEq       : value * value -> bool
    val compare     : value * value -> order

    val prettyValue : value -> string
    val prettyLabel : label -> string
end

structure Temp :> TEMP = struct

    type value  = int (* 2ʷ many variables on a w-sized machine       *)
		                  (* We can use `IntInf.int` if we want unbounded *)

    type label  = string

    val DUMMY_VALUE = ~1  (* Dummy value *)

    (* Already allocated special temporary values *)
    val argTemp1     = 1
    val argTemp2     = 2
    val resultTemp   = 3
    val framePointer = 4
    val stackPointer = 5
    val returnAddr   = 6
    val returnValue  = 7

    (* Make sure to update the below count based on the special temp vals assigned *)
    val curValue : value ref = ref 8 (* Keeps track of how many temps have been allocated   *)
    val curLabel : int ref   = ref 0 (* Keeps track of how many strings have been allocated *)

    (* Allocates a new `Temp.value` *)
    (* newValue : unit  -> value *)
    fun newValue () = let
                        val oldValue = !curValue
                     in
                        (curValue := oldValue + 1); oldValue
                     end

    (* Allocates a new `Temp.label` *)
    (* newLabel : unit  -> label *)
    fun newLabel () = let
                        val oldLabel = !curLabel
                      in
                        (curLabel := oldLabel + 1); "_TL" ^ (Int.toString oldLabel)
                      end

    (* Converts a string to a `Temp.label` *)
    (* strToLabel : string -> label *)
    fun strToLabel (s : string) = "_TSL" ^ "_" ^ s

    (* Check if two values are equal *)
    (* areEq : value * value -> bool *)
    fun areEq (v1, v2) = (v1 = v2)

    (* Compare two values *)
    (* compare : value * value -> order *)
    fun compare (v1, v2) = Int.compare (v1, v2)

    (* Pretty prints `Temp.value` *)
    (* prettyValue : value -> string *)
    fun prettyValue (t: value) = "_TV" ^ (Int.toString t)

    (* Pretty prints `Temp.label` *)
    (* prettyLabel : label -> string *)
    fun prettyLabel (t: label) = t
end
