structure MipsAssem = struct

structure F = MipsFrame
val ZERO = F.getSpecialReg "ZERO"

fun genR (name: string, dst: Temp.temp list, src: Temp.temp list, shamt: int option) format =
    if Utils.inList(["add", "sub", "addu", "subu", "and", "or", "xor", "nor", "slt", "sltu", "sllv", "srlv", "srav"], name)
    then
        let
            val rd = format(hd dst)
            val rs = format(hd src)
            val rt = format(hd (tl src))
        in
            if Utils.inList(["sllv", "srlv", "srav"], name)
                           (* These instructions have their src operands flipped... *)
            then name ^ " $" ^ rd ^ ", $" ^ rt ^ ", $" ^ rs ^ "\n"
            else name ^ " $" ^ rd ^ ", $" ^ rs ^ ", $" ^ rt ^ "\n"
        end
    else if Utils.inList(["sll", "srl", "sra"], name)
    then
        let
            val shamt = case shamt of SOME(i) => i
                                    | NONE => (Utils.debug("Compiler error: shamt not given for inst " ^ name);
                                               0)
            val rd = format(hd dst)
            val rt = format(hd src)
        in
            name ^ " $" ^ rd ^ ", $" ^ rt ^ ", " ^ Int.toString shamt ^ "\n"
        end
    else if Utils.inList(["jr", "mthi", "mtlo"], name)
    then
        let
            val rs = format(hd src)
        in
            name ^ " $" ^ rs ^ "\n"
        end
    else if Utils.inList(["mfhi", "mflo"], name)
    then
        let val rd = format(hd dst)
        in
            name ^ " $" ^ rd ^ "\n"
        end
    else if Utils.inList(["mult", "multu", "div", "divu"], name)
    then
        let val rs = format(hd src)
            val rt = format(hd (tl src))
        in
            name ^ " $" ^ rs ^ ", $" ^ rt ^ "\n"
        end
    else (Utils.debug("Compiler error: unknown R-instruction: " ^ name);
          "<ERROR>")

fun genI (name, dst: Temp.temp list, src: Temp.temp list, imm: int) format =
    if Utils.inList(["addi", "addiu", "slti", "sltiu", "andi", "ori", "xori"], name)
    then
        let val rt = format(hd dst)
            val rs = format(hd src)
        in
            name ^ " $" ^ rt ^ ", $" ^ rs ^ ", " ^ Int.toString imm ^ "\n"
        end
    else if Utils.inList(["lui"], name)
    then
        let val rt = format(hd dst)
        in
            name ^ " $" ^ rt ^ ", " ^ Int.toString imm ^ "\n"
        end
    else if Utils.inList(["lb", "lh", "lw", "lbu", "lhu"], name)
    then
        let val rt = format(hd dst)
            val rs = format(hd src)
        in
            name ^ " $" ^ rt ^ ", " ^ Int.toString imm ^ "($" ^ rs ^ ")" ^ "\n"
        end
    else if Utils.inList(["sb", "sh", "sw"], name)
    then
        (* Stores are special because they don't clobber registers *)
        let val rt = format(hd src)
            val rs = format(hd (tl src))
        in
            name ^ " $" ^ rt ^ ", " ^ Int.toString imm ^ "($" ^ rs ^ ")" ^ "\n"
        end
    else
        (Utils.debug("Compiler error: unknown I-instruction: " ^ name);
         "<ERROR>")
fun genJ (name, lab) format =
    if Utils.inList(["j", "jal"], name)
    then name ^ " " ^ Symbol.name lab ^ "\n"
    else (Utils.debug("Compiler error: unknown J-instruction: " ^ name);
          "<ERROR>")

fun genB (name, src, lab) format =
    if Utils.inList(["blez", "bgtz"], name)
    then
        let val rs = format(hd src)
        in
            name ^ " $" ^ rs ^ ", " ^ genLabel lab
        end
    else if Utils.inList(["beq", "bne"], name)
    then
        let
            val rs = format(hd src)
            val rt = format(hd (tl src))
        in
            name ^ " $" ^ rs ^ ", $" ^ rt ^ ", " ^ genLabel lab
        end
    else (Utils.debug("Compiler error: unknown branch instruction: " ^ name);
          "<ERROR>")
and genMove (temp1, temp2) format =
    (* Moves temp2 into temp1 *)
    "add $" ^ format temp1 ^ ", $" ^ format temp2 ^ ", $" ^ format ZERO
and genLabel l = Symbol.name l ^ "\n"
end
