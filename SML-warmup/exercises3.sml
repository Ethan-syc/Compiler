functor F(M: ORD_MAP where type Key.ord_key = string)
         (S:ORD_SET where type Key.ord_key = string) :>
        sig
            val proc: string list -> S.set M.map
        end
=
struct
fun proc filenameList =
    let
        (*
        To read the file and seperate the file content with space
        input: filename
        output: a tuple of a list of word and a filename
        *)
        fun readFileToList (filename:string):(string list * string) =
            let
                val file = TextIO.openIn filename
                val content = TextIO.inputAll file
                val _ = TextIO.closeIn file
            in (String.tokens Char.isSpace content, filename)
            end

        (*
        To insert the word derived from filename into Map
        input: a tuple of a list of word and a filename
        output: a Map which has Set as value
        *)
        fun insertListToMap (wordList, filename):(S.set M.map) =
            foldr (fn (word, map) => M.insert(map, word, S.add(S.empty, filename)))
                  M.empty wordList

        (*
        To union a list of Map that derived from different file
        input: a list of Map
        output: one single Map
         *)
        fun unionListOfMap listOfMap =
            foldr (fn (map1, map2) => M.unionWith S.union (map1, map2))
                  M.empty listOfMap
    in unionListOfMap (map insertListToMap (map readFileToList filenameList))
    end
end

structure TestMap = BinaryMapFn(struct type ord_key = string
  									                   val compare = String.compare
  								              end)

structure TestSet = BinarySetFn(struct type ord_key = string
  									                   val compare = String.compare
  								              end)

structure StringProcessor = F (TestMap) (TestSet)
