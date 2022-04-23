(* Signature of the Graph *)
signature GRAPH =
sig
	type node
	type 'a graph

	val empty           : unit -> 'a graph
	val newNode         : 'a graph -> 'a  -> node
	val addEdge         : 'a graph -> (node * node) -> unit
	val getSuccessors   : 'a graph -> node -> node list
	val getPredecessors : 'a graph -> node -> node list
	val numNodes        : 'a graph -> int
	val numEdges        : 'a graph -> int
	val label           : 'a graph -> node -> 'a
	val all             : 'a graph -> node list
	val alli            : 'a graph -> (node * 'a) list
	val map             : ('a -> 'b) -> 'a graph -> 'b graph
	val fold            : (('a * 'b) -> 'b) -> 'b -> 'a graph -> 'b
	val clear           : 'a graph -> unit
end

(* MkGraph functor to represent a graph *)
functor MkGraph () :> GRAPH =
struct
	type node = Word.word	(* Type of node *)

	(* val hashNode : node -> word *)
	fun hashNode (n : node) : Word.word = n

	(* val sameNode : (node * node) -> bool *)
	fun sameNode (n1 : node, n2 : node) : bool = (n1 = n2)

	(* Structure for NodeHashKey to make NodeHashSet *)
	structure NodeHashKey : HASH_KEY =
	struct
		type hash_key = node
		fun hashVal w = hashNode w
		fun sameKey (w1, w2) = sameNode (w1, w2)
	end

	(* NodeHashSet structure *)
	structure NodeHashSet = HashSetFn (NodeHashKey)

	(* Type of NodeHashSet *)
	type nodeHashSet = NodeHashSet.set

	exception NodeNotFound	(* Exception for HashTable *)

	type 'a graph = {
						labels      : (node, 'a) HashTable.hash_table,
						successors  : (node, nodeHashSet) HashTable.hash_table,
						predecessors: (node, nodeHashSet) HashTable.hash_table,
						numEdges    : int ref,
						nextNode    : node ref
					}

	val initSize  = 100	(* Initial size *)
	val nodeStart = 0	(* First Node *)

	(* val empty : unit -> 'a graph *)
	(* Creates an empty graph *)
	fun empty () = {
		labels       = HashTable.mkTable (hashNode, sameNode) (initSize, NodeNotFound),
		successors   = HashTable.mkTable (hashNode, sameNode) (initSize, NodeNotFound),
		predecessors = HashTable.mkTable (hashNode, sameNode) (initSize, NodeNotFound),
		numEdges     = ref 0,
		nextNode     = ref (Word.fromInt nodeStart)
	}

	(* val newNode : 'a graph -> 'a  -> node *)
	(* Adds a new (node, label) to the graph *)
	fun newNode (g : 'a graph) (a : 'a) : node =
		let
			val labels       = #labels g
			val successors   = #successors g
			val predecessors = #predecessors g
			val nextNode     = #nextNode g
			val newNode      = !nextNode
			val n = Word.toInt (newNode)
			val _ = HashTable.insert labels (newNode, a)
			val _ = HashTable.insert successors (newNode, NodeHashSet.mkEmpty initSize)
			val _ = HashTable.insert predecessors (newNode, NodeHashSet.mkEmpty initSize)
			val _ = (nextNode := Word.fromInt (n + 1))
		in
			newNode
		end

	(* val addEdge : 'a graph -> (node * node) -> unit *)
	(* Adds a new edge to the graph *)
	fun addEdge (g : 'a graph) (n1 : node, n2 : node) : unit =
		let
			val successors   = #successors g
			val predecessors = #predecessors g
			val numEdges     = #numEdges g
			val n1HashSet    = HashTable.lookup successors n1
			val n2HashSet    = HashTable.lookup predecessors n2
		in
			(NodeHashSet.addc n1HashSet n2);
			(NodeHashSet.addc n2HashSet n1);
			(numEdges := !numEdges + 1)
		end

	(* val getSuccessors : 'a graph -> node -> node list *)
	(* Gets the successors of a node of the graph *)
	fun getSuccessors (g : 'a graph) (n : node) : node list =
		let
			val successors = #successors g
			val nHashSet   = HashTable.lookup successors n
		in
			NodeHashSet.listItems nHashSet
		end

	(* val getPredecessors : 'a graph -> node -> node list *)
	(* Gets the predecessors of a node of the graph *)
	fun getPredecessors (g : 'a graph) (n : node) : node list =
		let
			val predecessors = #predecessors g
			val nHashSet     = HashTable.lookup predecessors n
		in
			NodeHashSet.listItems nHashSet
		end

	(* val numNodes : 'a graph -> int *)
	(* Returns the number of nodes of the graph *)
	fun numNodes (g : 'a graph) : int = Word.toInt (!(#nextNode g))

	(* val numEdges        : 'a graph -> int *)
	(* Returns the number of edges of the graph *)
	fun numEdges (g : 'a graph) : int = !(#numEdges g)


	(* val label : 'a graph -> node -> 'a *)
	(* Get the label of a node *)
	fun label (g : 'a graph) (n : node) : 'a =
		let
			val labels = #labels g
		in
			HashTable.lookup labels n
		end

	(* val all : 'a graph -> node list *)
	(* Get all the nodes of a graph *)
	fun all (g : 'a graph) : node list = map (fn (x, y) => x) (HashTable.listItemsi (#labels g))

	(* val alli : 'a graph -> (node * 'a) list *)
	(* Get all the (node, label) pairs of a graph *)
	fun alli (g : 'a graph) : (node * 'a) list = HashTable.listItemsi (#labels g)

	(* val map : ('a -> 'b) -> 'a graph -> 'b graph *)
	(* Maps a function over the labels of the graph *)
	fun map (f : 'a -> 'b) (g : 'a graph) : 'b graph = {
		labels       = HashTable.map f (#labels g),
		successors   = HashTable.copy (#successors g),
		predecessors = HashTable.copy (#predecessors g),
		numEdges     = ref (!(#numEdges g)),
		nextNode     = ref (Word.fromInt (Word.toInt (!(#nextNode g))))
	}

	(* val fold : (('a * 'b) -> 'b) -> 'b -> 'a graph -> 'b *)
	(* Folds a function over the labels of the graph *)
	fun fold (f : ('a * 'b) -> 'b) (l0 : 'b) (g : 'a graph) : 'b = HashTable.fold f l0 (#labels g)

	(* val clear : 'a graph -> unit *)
	(* Clears the graph *)
	fun clear (g : 'a graph) : unit =
		let
			val labels       = #labels g
			val successors   = #successors g
			val predecessors = #predecessors g
			val nextNode     = #nextNode g
		in
			(HashTable.clear labels);
			(HashTable.clear successors);
			(HashTable.clear predecessors);
			(nextNode := Word.fromInt 0)
		end
end
