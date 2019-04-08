signature LIVENESS =
sig
  type igraph
  val interferenceGraph: Flow.flowgraph -> igraph
  val show: TextIO.outstream * igraph -> unit
end

structure Key: ORD_KEY =
struct
type ord_key = Temp.temp
val compare = Int.compare
end

structure LivenessEquation: FLOW_EQN =
struct
structure Set = BinarySetFn(Key)
structure Graph = Flow.Graph
fun inEqn (_, _, node, inSet, outSet) =
    let val {def, use, ismove} = Graph.nodeInfo node
        val useSet = Set.addList(Set.empty, use)
        val defSet = Set.addList(Set.empty, def)
    in
        Set.union(useSet, Set.difference(outSet, defSet))
    end
fun outEqn (inGraph, _, node, inSet, outSet) =
    let fun helper(nodeID, set) = Set.union(set, (Graph.nodeInfo (Graph.getNode(inGraph, nodeID))))
    in
        Graph.foldSuccs helper Set.empty node
    end

val initial = Set.empty

end

structure Liveness: LIVENESS =
struct

structure DF = Dataflow(LivenessEquation)
structure S = LivenessEquation.Set
structure FG = Flow.Graph
structure G = FuncGraph(Key)

datatype igraph =
         IGRAPH of {graph: S.set G.graph,
                    moves: (Temp.temp * Temp.temp) list}

fun interferenceGraph (Flow.FGRAPH {control=g}: Flow.flowgraph) =
    let val (inGraph, outGraph) = DF.compute(g)
        val moves = ref []
        fun addNode (node, graph) =
            let val {def, use, ismove} = FG.nodeInfo(node)
            in
                foldl (fn (nodeID, graph) => (G.addNode(graph, nodeID, S.empty))) graph (def@use)
            end
        val igraph = foldl addNode G.empty (FG.nodes g)
        fun addInterference(node, graph) =
            let val {def, use, ismove} = FG.nodeInfo(node)
                val outSet = FG.nodeInfo (FG.getNode (outGraph, FG.getNodeID node))
                fun addEdge ((dst, src), graph) = G.addEdge(graph, {from=src, to=dst})
            in
                if ismove
                then
                    let val dst = hd def
                        val src = hd use
                        val _ = moves := ((dst, src)::(!moves))
                        val outSet = S.delete (S.add(outSet, src), src)
                    in
                        S.foldl (fn(liveout, graph) => addEdge((dst, liveout), graph)) graph outSet
                    end
                else
                    let fun helper (dst, graph) =
                            S.foldl (fn(liveout, graph) => addEdge((dst, liveout), graph)) graph outSet
                    in
                        foldl helper graph def
                    end
            end
        val igraph = foldl addInterference igraph (FG.nodes g)
    in
        IGRAPH {graph=igraph, moves=(!moves)}
    end

fun show(out, IGRAPH {graph, moves}) =
    let fun printNode (nodeID, set) =
            Int.toString nodeID ^ ": " ^ (Utils.join "," (map Int.toString (S.listItems set)))
    in
        G.printGraph printNode out graph
    end

end
