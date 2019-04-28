signature FLOW_EQN =
sig
    structure Set: ORD_SET
    structure Graph: FUNCGRAPH
    val inEqn: (Set.set Graph.graph *
                Set.set Graph.graph *
                Flow.info Graph.node *
                Set.set *
                Set.set) -> Set.set
    val outEqn: (Set.set Graph.graph *
                 Set.set Graph.graph *
                 Flow.info Graph.node *
                 Set.set *
                 Set.set) -> Set.set
    val initial: Set.set
end

functor Dataflow(E: FLOW_EQN) =
struct

structure G = E.Graph
type Result = E.Set.set G.graph

fun compute(g: Flow.info G.graph): (Result * Result) =
    let fun createNode getValue (node, graph) =
            G.addNode(graph, G.getNodeID(node), getValue(node))
        val inGraph = foldl (createNode(fn(node) => E.initial)) G.empty (G.nodes g)
        val outGraph = foldl (createNode(fn(node) => E.initial)) G.empty (G.nodes g)
        val changed = ref true
        val inGraph = ref inGraph
        val outGraph = ref outGraph
    in
        while (!changed) do
              let val _ = changed := false
                  fun adjustNode (node, (inGraph, outGraph)) =
                      let val key = G.getNodeID(node)
                          val inSet = G.nodeInfo(G.getNode(inGraph, key))
                          val outSet = G.nodeInfo(G.getNode(outGraph, key))
                          val newIn = E.inEqn(inGraph, outGraph, node, inSet, outSet)
                          val newOut = E.outEqn(inGraph, outGraph, node, inSet, outSet)
                      in
                          if E.Set.equal(newIn, inSet) andalso E.Set.equal(newOut, outSet) then (inGraph, outGraph)
                          else (changed := true; (G.changeNodeData(inGraph, key, newIn),
                                                  G.changeNodeData(outGraph, key, newOut)))
                      end
                  val (newIn, newOut) = foldl adjustNode ((!inGraph), (!outGraph)) (G.nodes g)
                  val _ = inGraph := newIn
                  val _ = outGraph := newOut
              in
                  ()
              end;
        (!inGraph, !outGraph)
    end

end
