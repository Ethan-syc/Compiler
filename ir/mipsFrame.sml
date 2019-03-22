structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
(* TODO book says numLocals, for what? *)
type frame = {label:Temp.label, formals: access list, offset: int ref, numLocals: int ref}

(* TODO how to do this? *)
val SP = Temp.newtemp()
val FP = Temp.newtemp()

fun newFrame {name: Temp.label, formals} =
    let
        val offsetPos = ref 4   (* This is offset from FP, plus 4 for the static link *)
        fun buildFormals (formals:bool list):access list =
            case formals of [] => []
                          (* not escape -> register *)
                          | false::rest => (InReg(Temp.newtemp()))::(buildFormals rest)
                          (* escape -> in frame *)
                          | true::rest =>
                            let
                                val offset = !offsetPos
                                val _ = offsetPos := !offsetPos + 4
                            in
                                (InFrame offset)::(buildFormals rest)
                            end
        (* InFrame 0 is the static link *)
        val formals' = (InFrame 0)::(buildFormals formals)
    in
        {label=name, formals=formals', offset=ref 0, numLocals=ref 0}
    end

fun name {label, formals, offset, numLocals} = label

fun formals {label, formals, offset, numLocals} = formals

(* Allocates new local variable in the frame *)
fun allocLocal {label, formals, offset, numLocals} escape =
    if not escape
    then (numLocals := !numLocals + 1; InReg(Temp.newtemp()))
    else
        let
            val _ = numLocals := !numLocals + 1
            (* grows down *)
            val _ = offset := !offset - 4
        in
            InFrame(!offset)
        end
end
