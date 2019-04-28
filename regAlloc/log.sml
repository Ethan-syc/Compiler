structure Log =
struct

(* Logging *)
exception Log
val DEBUG = 1
val VERBOSE = 2
val INFO = 3
val WARNING = 4
val ERROR = 5
val NOLOG = 6
(* todo: change this to disable logging *)
val loglevel = ref ERROR

fun loglevelToName 1 = "DEBUG"
  | loglevelToName 2 = "VERBOSE"
  | loglevelToName 3 = "INFO"
  | loglevelToName 4 = "WARNING"
  | loglevelToName 5 = "ERROR"
  | loglevelToName _ = raise Log

fun log level msg =
    if level >= (!loglevel) then
        let val date = Date.fromTimeLocal(Time.now())
            val levelname = loglevelToName level
        in
            print((Date.fmt "%c" date) ^ " " ^ levelname ^ ": " ^ msg ^ "\n")
        end
    else ()

val debug = log DEBUG
val verbose = log VERBOSE
val info = log INFO
val warning = log WARNING
val error = log ERROR

end
