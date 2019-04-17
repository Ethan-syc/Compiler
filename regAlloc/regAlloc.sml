signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc :  KeySet.set KeyGraph.graph -> allocation (* frame ? *)
  val showAllocations : allocation * KeySet.set KeyGraph.graph * TextIO.outstream -> unit list
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = MipsFrame
  structure G = KeyGraph
  type allocation = Frame.register Temp.Table.table
  val initial = Frame.tempMap
  val callersaves = Frame.getSpecialRegs("callersaves")
  val calleesaves = Frame.getSpecialRegs("calleesaves")
  val argregs = Frame.getSpecialRegs("argregs")
  val RV = Frame.getSpecialReg("RV")
  val V1 = Frame.getSpecialReg("v1")
  val registers = callersaves @ calleesaves @ argregs @ [RV] @ [V1]
  val registerStrings = map (MipsFrame.regToString initial) registers
  fun dummySpillCost (temp) = 1
  fun alloc (interference) =
    let
      val (allocation, spillList) = Color.color {interference=interference, initial=initial, spillCost=dummySpillCost, registers=registerStrings}
    in
      allocation
    end

  fun showAllocations (allocation, interference, out) =
    let
      fun showAllocation (temp) =
        let
          val color = case Temp.Table.look(allocation, temp) of SOME(color) => color
                                                        | NONE => (Log.error("fail to find color for this temp"); "no color")
        in
          (TextIO.output(out, "register " ^ Int.toString (temp) ^ " uses " ^ color ^ "\n"))
        end
      val _ = TextIO.output(out, "================ RegAlloc ===============\n")
      val temps = map G.getNodeID (G.nodes(interference))
    in
      map showAllocation temps
    end
end
