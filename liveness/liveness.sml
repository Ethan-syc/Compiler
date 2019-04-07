signature LIVENESS =
sig
  type igraph
  val interferenceGraph: Flow.flowgraph -> igraph * (Flow.info Flow.Graph.node -> Temp.temp list)
  val show: TextIO.outstream * igraph -> unit
end
