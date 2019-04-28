structure Assem = struct

  type reg = string
  type temp = Temp.temp
  type label = Temp.label

  datatype instr = OPER of {assem: (temp -> string) -> string,
                  			    dst: temp list,
                  			    src: temp list,
                  			    jump: label list option}
                 | LABEL of {assem: string, lab: Temp.label}
                 | MOVE of {assem: (temp -> string) -> string,
                  			    dst: temp,
                  			    src: temp}

end
