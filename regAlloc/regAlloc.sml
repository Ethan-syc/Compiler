signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc :  KeySet.set KeyGraph.graph * Frame.frame -> allocation
  val showAllocations : allocation * Temp.temp list * TextIO.outstream -> unit list
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = MipsFrame
  structure G = KeyGraph
  structure T = Temp.Table
  type allocation = Frame.register Temp.Table.table
  val initial = Frame.tempMap
  val callersaves = Frame.getSpecialRegs("callersaves")
  val calleesaves = Frame.getSpecialRegs("calleesaves")
  val argregs = Frame.getSpecialRegs("argregs")
  val RV = Frame.getSpecialReg("RV")
  val V1 = Frame.getSpecialReg("v1")
  val registers = callersaves @ calleesaves @ argregs @ [RV] @ [V1]
  val registerStrings = map (MipsFrame.regToString initial) registers
  fun dummySpillCost (temp) =
      (* If precolored, return Int.maxInt *)
      if isSome(T.look(initial, temp)) then 1073741823 else 1
  fun alloc (interference, frame) =
    let
      val (allocation, spillList) = Color.color {interference=interference, initial=initial, spillCost=dummySpillCost, registers=registerStrings}
    in
      allocation
    end

  fun showAllocations (allocation, temps, out) =
    let
        fun showAllocation (temp) =
            case T.look(allocation, temp)
             of SOME(color) =>
                TextIO.output(out, "register " ^ Int.toString (temp) ^ " uses " ^ color ^ "\n")
              | _ => Log.error ("Register " ^ Int.toString temp ^ " not found!")
        val _ = TextIO.output(out, "================ RegAlloc ===============\n")
    in
        map showAllocation temps
    end
end
