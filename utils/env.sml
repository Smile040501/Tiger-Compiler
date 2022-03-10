(* Structure to create an Environment *)
signature ENV =
sig
    type key = string
    type value
    type mp
    val empty    : unit -> mp
    val find     : mp -> key -> value option
    val insert   : mp -> key -> value -> mp
    val inDomain : mp -> key -> bool
end

structure Env :> ENV =
struct
    type key   = string
    type value = Temp.value
    type mp    = value AtomMap.map

    fun empty    ()    = AtomMap.empty

    and find     m k   = AtomMap.find (m, Atom.atom k)

    and insert   m k v = AtomMap.insert (m, Atom.atom k, v)

    and inDomain m k   = AtomMap.inDomain (m, Atom.atom k)
end
