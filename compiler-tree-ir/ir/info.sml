(* Structure to store information which will be required while translation *)
structure Info =
struct
    (* Loop Label can useful in case of `break` and `continue` statements *)
    datatype Info = Info of {
        loopLabel : Temp.label option,
        currFrame : Frame.Frame,
        currFunc  : Tiger.id,
        numAllocs : int
    }

    (* Create a new Info *)
    val emptyInfo = Info {
        loopLabel = NONE,
        currFrame = Frame.emptyFrame,
        currFunc = "main",
        numAllocs = 0
    }

    (* Getter utility functions *)
    fun getLoopLabel (Info i) = #loopLabel i
    fun getCurrFrame (Info i) = #currFrame i
    fun getCurrFunc  (Info i) = #currFunc  i
    fun getNumAllocs (Info i) = #numAllocs i

    (* Create a new Info with a new loop label *)
    fun setLoopLabel (Info {loopLabel, currFrame, currFunc, numAllocs}) newOptLabel =
            Info {
                loopLabel = newOptLabel,
                currFrame = currFrame,
                currFunc = currFunc,
                numAllocs = numAllocs
            }

    (* Create a new Info with a new frame *)
    fun setCurrFrame (Info {loopLabel, currFrame, currFunc, numAllocs}) newCurrFrame =
            Info {
                loopLabel = loopLabel,
                currFrame = newCurrFrame,
                currFunc = currFunc,
                numAllocs = numAllocs
            }

    (* Create a new Info with a new function *)
    fun setCurrFunc (Info {loopLabel, currFrame, currFunc, numAllocs}) newCurrFunc =
            Info {
                loopLabel = loopLabel,
                currFrame = currFrame,
                currFunc = newCurrFunc,
                numAllocs = numAllocs
            }

    (* Sets the number of allocated variables in the info *)
    fun setNumAllocs (Info {loopLabel, currFrame, currFunc, numAllocs}) cnt =
            Info {
                loopLabel = loopLabel,
                currFrame = currFrame,
                currFunc = currFunc,
                numAllocs = cnt
            }

    (* Create a new Info with a new number of allocated variables *)
    fun updateNumAllocs (i as Info r) cnt = setNumAllocs i ((#numAllocs r) + cnt)

    (* Resets the number of allocated variables in the info *)
    fun resetNumAllocs (i : Info) = setNumAllocs i 0

end
