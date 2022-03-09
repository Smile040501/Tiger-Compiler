(* Structure to create an Environment *)
signature ENV =
sig
    type value
    type mp
    val empty  : unit -> mp
    val lookup : mp -> string -> value option
    val update : mp -> string -> value -> mp
end

structure Env :> ENV =
struct
    type value = Temp.temp
    type mp    = value AtomMap.map

    fun empty  ()      = AtomMap.empty
    fun lookup m key   = AtomMap.find (m, Atom.atom key)
    fun update m key v = AtomMap.insert (m, Atom.atom key, v)
end
