signature FRAME =
sig
  type frame
  (* in frame or in reg *)
  type access
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  (* formals is a bool list indicate if param escapes,
     new frame calculate 2 things:
     how the param will be seen from inside the function (in reg/frame)
     what insturction must be produced to implement the view shift (copy stack pointer to frame pointer) *)
  val FP : Temp.temp
  val SP: Temp.temp
  val RV : Temp.temp
  val newFrame : {name: Temp.label,
                  formals: bool list} -> frame
  val name : frame -> Temp.label
  (* extract formals locaiton as seen from the callee *)
  val formals : frame -> access list
  val numLocals: frame -> int
  val allocLocal : frame -> bool -> access
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall: string * Tree.exp list -> Tree.exp
  val procEntryExit1: frame * Tree.exp -> frag
  val procEntryExit3: frame * Tree.exp -> frag
  val allocString: Temp.label * string -> frag
  val printFrag: TextIO.outstream * frag -> unit
  val tempMap: Temp.temp Temp.Table.table
  val string: Temp.label * string -> string
  val regToString: Temp.temp -> string
  val getSpecialReg: string -> Temp.temp
  val getSpecialRegs: string -> Temp.temp list
  val arg: int -> Tree.exp
end
