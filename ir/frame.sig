signature FRAME =
sig
  type frame
  (* in frame or in reg *)
  type access
  (* formals is a bool list indicate if param escapes,
  new frame calculate 2 things:
  how the param will be seen from inside the function (in reg/frame)
  what insturction must be produced to implement the view shift (copy stack pointer to frame pointer)
  *)
  val newFrame : {name: Temp.label,
                  formals: bool list} -> frame
  val name : frame -> Temp.label
  (* extract formals locaiton as seen from the callee *)
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
end
