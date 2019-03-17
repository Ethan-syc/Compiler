structure Env :> ENV =
struct
  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty, loopVar: bool}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv =
    let
      val baseTypeList = [(Symbol.symbol "int", Types.INT),
                           (Symbol.symbol "string", Types.STRING)]
      fun insert ((symb, ty), table) = Symbol.enter(table, symb, ty)
    in
      foldl insert Symbol.empty baseTypeList
    end

  val base_venv =
    let
      val baseFunList = [(Symbol.symbol "print", FunEntry {formals=[Types.STRING], result=Types.UNIT}),
                           (Symbol.symbol "flush", FunEntry {formals=[], result=Types.UNIT}),
                           (Symbol.symbol "getchar", FunEntry {formals=[], result=Types.STRING}),
                           (Symbol.symbol "ord", FunEntry {formals=[Types.STRING], result=Types.INT}),
                           (Symbol.symbol "chr", FunEntry {formals=[Types.INT], result=Types.STRING}),
                           (Symbol.symbol "size", FunEntry {formals=[Types.STRING], result=Types.INT}),
                           (Symbol.symbol "substring", FunEntry {formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}),
                           (Symbol.symbol "concat", FunEntry {formals=[Types.STRING, Types.STRING], result=Types.STRING}),
                           (Symbol.symbol "not", FunEntry {formals=[Types.INT], result=Types.INT}),
                           (Symbol.symbol "exit", FunEntry {formals=[Types.INT], result=Types.UNIT})
                           ]
      fun insert ((symb, enventry), table) = Symbol.enter(table, symb, enventry)
    in
      foldl insert Symbol.empty baseFunList
    end
end
