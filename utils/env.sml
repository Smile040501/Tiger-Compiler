(* Structure to create an Environment *)
(* It is a mapping from user variables (string) to Temporary values (Temp.value) *)
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

    (* Returns an empty environment *)
    (* empty : unit -> mp *)
    fun empty    ()       = AtomMap.empty

    (* Looks for a key in the map and returns the value as option *)
    (* find : mp -> key -> value option *)
    fun find     m k      = AtomMap.find (m, Atom.atom k)

    (* Looks for a key in the map and returns the value, raise error if not found *)
    (* lookup : mp -> key -> value *)
    fun lookup   m k      = AtomMap.lookup (m, Atom.atom k)

    (* Inserts a key-value pair in the environment *)
    (* insert : mp -> key -> value -> mp *)
    fun insert   m k v    = AtomMap.insert (m, Atom.atom k, v)

    (* Checks if a key is present in the environement or not *)
    (* inDomain : mp -> key -> bool *)
    fun inDomain m k      = AtomMap.inDomain (m, Atom.atom k)

    (* Tells what to do for values on the union over same keys *)
    (* unionFun : (value * value) -> value *)
    fun unionFun (v1, v2) = (case Temp.areEq (v1, v2) of
                                  true  => v1
                                | false => (Utils.throwErr MergingEnvironmentConflict "[env.sml]:[unionFun]: Environments' key values are not equal of all the same keys"))

    (* Takes union of two environments *)
    (* union : mp -> mp  -> mp *)
    fun union    m1 m2    = AtomMap.unionWith unionFun (m1, m2)

    (* Returns a list of key-value pairs in the environment *)
    (* listItems : mp -> (key * value) list *)
    fun listItems m       = map (fn (k, v) => (Atom.toString k, v)) (AtomMap.listItemsi m)
end
