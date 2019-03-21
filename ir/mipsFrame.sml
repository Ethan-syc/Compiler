structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {label:Temp.label, formals: access list, localVarNum: int}
  
  fun newFrame {name, formals} = {label=name, formals=[], localVarNum=0}
  fun name _ = Temp.newlabel()
  fun formals _ = []
  fun allocLocal _ _ = InFrame 0
end
