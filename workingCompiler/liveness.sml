signature LIVENESS =
sig
  type igraph
  type actualGraph
  val interferenceGraph: Flow.flowgraph -> igraph
  val getInterference: igraph -> actualGraph
  val show: TextIO.outstream * igraph -> unit
end

structure LivenessEquation: FLOW_EQN =
struct
structure K = Key
structure Set = KeySet
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
structure S = KeySet
structure FG = Flow.Graph
structure K = Key
structure G = KeyGraph


datatype igraph =
         IGRAPH of {graph: S.set G.graph,
                    inGraph: DF.Result,
                    outGraph: DF.Result,
                    moves: (Temp.temp * Temp.temp) list}
type actualGraph = S.set G.graph

fun getInterference (IGRAPH {graph, inGraph, outGraph, moves}) = graph

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
                fun addEdge ((src, dst), graph) = G.doubleEdge(graph, src, dst)
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
        IGRAPH {graph=igraph, inGraph=inGraph, outGraph=outGraph, moves=(!moves)}
    end

fun show(out, IGRAPH {graph, inGraph, outGraph, moves}) =
    let fun printNode (nodeID, set) =
            Int.toString nodeID ^ ": {" ^ (Utils.join "," (map Int.toString (S.listItems set))) ^ "}"
        fun printIONode (nodeID, set) =
            nodeID ^ ": {" ^ (Utils.join "," (map Int.toString (S.listItems set))) ^ "}"
    in
        G.printGraph printNode out graph
        (* TextIO.output(out, "============== In Graph ==============\n"); *)
        (* FG.printGraph printIONode out inGraph; *)
        (* TextIO.output(out, "============== Out Graph ==============\n"); *)
        (* FG.printGraph printIONode out outGraph *)
    end

end
