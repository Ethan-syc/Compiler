structure Test =
struct

val testcaseDir = OS.Path.mkAbsolute {path="../tiger/testcases/", relativeTo=OS.FileSys.getDir()}
fun test i =
    let val filename = OS.Path.concat (testcaseDir, "test" ^ Int.toString i ^ ".tig")
        val _ = print("Testing " ^ filename ^ "\n")
        val _ = Main.compile filename
    in
        TextIO.input(TextIO.stdIn);
        test(i + 1)
    end
end
