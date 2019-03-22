signature TRANSLATE =
sig
    type level
    type access
    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
end

structure Translate =
struct

fun err(pos,message) = ErrorMsg.error pos message

type exp = unit

datatype level = INNER of {frame: MipsFrame.frame, parent: level}
       | OUTERMOST

type access = level * MipsFrame.access

val outermost = OUTERMOST

(* need to add one true to represent static link, TODO but how to get the link *)
fun newLevel {parent=level, name=name, formals} =
    let
        val newFormals = true::formals
        val newFrame = MipsFrame.newFrame {name=name, formals=newFormals}
    in
        INNER {frame = newFrame, parent = level}
    end

fun formals level =
    case level of OUTERMOST => []
                | INNER {frame, parent} =>
                  foldl (fn(FrameAccess, curResult) => (level, FrameAccess)::curResult) [] (MipsFrame.formals frame)

fun allocLocal level escape =
    case level of OUTERMOST =>
                  (err(0, "can't allocate Local varibale in the OUTERMOST level");
                   (OUTERMOST, (MipsFrame.allocLocal (MipsFrame.newFrame {name=Temp.newlabel(), formals=[]}) escape)))
                | INNER {frame, parent} => (INNER {frame=frame, parent=parent}, (MipsFrame.allocLocal frame escape))
end
