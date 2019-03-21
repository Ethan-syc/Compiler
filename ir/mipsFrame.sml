structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  (* TODO book says localVarNum, for what? *)
  type frame = {label:Temp.label, formals: access list, offset: int ref, localVarNum: int ref}

  (* TODO how to do this? *)
  val SP = Temp.newtemp()
  val FP = Temp.newtemp()

  fun newFrame {name: Temp.label, formals} =
    let
      val offsetPos = ref 0
      fun buildFormals (formals:bool list):access list =
        case formals of [] => []
                      (* not escape -> register *)
                      | false::rest => (InReg(Temp.newtemp()))::(buildFormals rest)
                      (* escape -> in frame *)
                      | true::rest =>
                        let
                          val offset = !offsetPos
                          val _ = offsetPos := !offsetPos+4
                        in
                          (InFrame offset)::(buildFormals rest)
                        end
    in
      {label=name, formals=buildFormals formals, offset=ref 0, localVarNum=ref 0}
    end

  fun name {label, formals, offset, localVarNum} = label

  fun formals {label, formals, offset, localVarNum} = formals

  fun allocLocal {label, formals, offset, localVarNum} escape =
    if escape
      then (localVarNum := !localVarNum + 1; InReg(Temp.newtemp()))
      else
        let
          val _ = localVarNum := !localVarNum + 1
          (* grows down *)
          val _ = offset := !offset - 4
          val frameAccess = InFrame (!offset)
        in
          frameAccess
        end
end
