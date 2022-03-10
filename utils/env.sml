(* Structure to create an Environment *)
signature ENV =
sig
    type key   = string
    type value = Temp.value
    type mp
    val empty     : unit -> mp
    val find      : mp -> key -> value option
    val lookup    : mp -> key -> value
    val insert    : mp -> key -> value -> mp
    val inDomain  : mp -> key -> bool
    val union     : mp -> mp  -> mp
    val listItems : mp -> (key * value) list
end

structure Env :> ENV =
struct
    type key   = string
    type value = Temp.value
    type mp    = value AtomMap.map

    exception MergingEnvironmentConflict of string

    fun empty    ()       = AtomMap.empty

    fun find     m k      = AtomMap.find (m, Atom.atom k)

    fun lookup   m k      = AtomMap.lookup (m, Atom.atom k)

    fun insert   m k v    = AtomMap.insert (m, Atom.atom k, v)

    fun inDomain m k      = AtomMap.inDomain (m, Atom.atom k)

    fun unionFun (v1, v2) = (case Temp.areEq (v1, v2) of
                                  true  => v1
                                | false => (raise MergingEnvironmentConflict "Environment key values are not equal"))

    fun union    m1 m2    = AtomMap.unionWith unionFun (m1, m2)

    fun listItems m       = map (fn (k, v) => (Atom.toString k, v)) (AtomMap.listItemsi m)
end
