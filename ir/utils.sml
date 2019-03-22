structure Utils =
struct

(* A specialization of List.exists for bool lists *)
fun anyOf l = List.exists (fn(x) => x) l

end
