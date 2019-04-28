signature COLOR =
sig
    structure Frame: FRAME
    exception spill of Temp.temp
    type allocation = Frame.register Temp.Table.table
    val color: {interference: KeySet.set KeyGraph.graph,
                initial: allocation,
                spillCost: Temp.temp -> int,
                registers: Frame.register list}
               -> allocation * Temp.temp list
end

structure Color:COLOR =
struct
exception spill of Temp.temp
exception impossible
structure Frame = MipsFrame
type allocation = Frame.register Temp.Table.table
structure K = Key
structure S = KeySet
structure G = KeyGraph
structure L = Liveness
structure T = Temp.Table
(* for chosing color *)
structure ColorSet = BinarySetFn(
    struct
    type ord_key = Frame.register
    val compare = String.compare
    end)

fun isPreColored temp = isSome(T.look(Frame.tempMap, temp))

fun findNodeToSimplify ([], trivialDegree) = NONE
  | findNodeToSimplify(node::rest, trivialDegree) =
    if isPreColored (G.getNodeID node)
    then findNodeToSimplify(rest, trivialDegree)
    else if (G.inDegree(node)) < trivialDegree
    then SOME(node)
    else findNodeToSimplify(rest, trivialDegree)

fun getOneFreeColor(neighborColorsSet, registers) =
    let
        val neighborColorsList = ColorSet.listItems(neighborColorsSet)
        val freeColorList = foldr (fn (color, curResult) =>
                                      let
                                          val used = ColorSet.member(neighborColorsSet, color)
                                      in
                                          if used then curResult else color::curResult
                                      end
                                  ) [] registers
    in
        hd freeColorList
    end

fun getOptional (SOME color) = color
  (* will raise spill earlier, so this is impossible to happen *)
  | getOptional (NONE) = (Log.error "can't find the color"; raise impossible)

fun getColor allocation id =
    case T.look(allocation, id)
     of SOME(color) => color
      | NONE =>
        (case T.look(Frame.tempMap, id) of SOME(color) => color
                                         | _ => (Log.error("can't find color for ID " ^ Int.toString id);
                                                 raise impossible))

fun color {interference, initial, spillCost, registers} =
    let
        val trivialDegree = length(registers)
        val nodes = G.nodes(interference)
        val node = findNodeToSimplify(nodes, trivialDegree)
        val newAllocation =
            case node
             of NONE =>
                let
                    val temps = map G.getNodeID nodes
                    val allPreColored = Utils.allOf (map isPreColored temps)
                    val result = if allPreColored then initial
                                 else
                                     let val min = Utils.min spillCost temps
                                     val _ = print("spill!")
                                     in
                                         raise spill min
                                     end
                in
                    result
                end
              | SOME(node) =>
                let
                    val _ = Log.debug("Simplifying temp " ^ Int.toString (G.getNodeID node))
                    val neighbors = G.succs node
                    val newGraph = G.removeNode (interference, G.getNodeID node)
                    val (allocation, spills) = color {interference=newGraph,
                                                      initial=initial,
                                                      spillCost=spillCost,
                                                      registers=registers}
                    val neighborColors = foldl ColorSet.add' ColorSet.empty (map (getColor allocation) neighbors)
                    val _ = Log.debug("Neighbor colors for temp "
                                      ^ Int.toString (G.getNodeID node)
                                      ^ " is "
                                      ^ Utils.join ", " (ColorSet.listItems neighborColors))
                    val colorToAssign = getOneFreeColor(neighborColors, registers)
                    val _ = Log.debug("Assigned " ^ colorToAssign ^ " to " ^ Int.toString (G.getNodeID node))
                    val newAllocation = T.enter(allocation, G.getNodeID node, colorToAssign)
                in
                    newAllocation
                end
    in
        (newAllocation, [])
    end
end
