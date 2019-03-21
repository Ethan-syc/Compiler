structure Env :> ENV =
struct
  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty, loopVar: bool}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: ty list, result: ty}
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
      val baseFunList = [(Symbol.symbol "print", FunEntry {level=Translate.outermost, label=Temp.namedlabel("print"), formals=[Types.STRING], result=Types.UNIT}),
                           (Symbol.symbol "flush", FunEntry {level=Translate.outermost, label=Temp.namedlabel("flush"), formals=[], result=Types.UNIT}),
                           (Symbol.symbol "getchar", FunEntry {level=Translate.outermost, label=Temp.namedlabel("getchar"), formals=[], result=Types.STRING}),
                           (Symbol.symbol "ord", FunEntry {level=Translate.outermost, label=Temp.namedlabel("ord"), formals=[Types.STRING], result=Types.INT}),
                           (Symbol.symbol "chr", FunEntry {level=Translate.outermost, label=Temp.namedlabel("chr"), formals=[Types.INT], result=Types.STRING}),
                           (Symbol.symbol "size", FunEntry {level=Translate.outermost, label=Temp.namedlabel("size"), formals=[Types.STRING], result=Types.INT}),
                           (Symbol.symbol "substring", FunEntry {level=Translate.outermost, label=Temp.namedlabel("substring"), formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}),
                           (Symbol.symbol "concat", FunEntry {level=Translate.outermost, label=Temp.namedlabel("concat"), formals=[Types.STRING, Types.STRING], result=Types.STRING}),
                           (Symbol.symbol "not", FunEntry {level=Translate.outermost, label=Temp.namedlabel("not"), formals=[Types.INT], result=Types.INT}),
                           (Symbol.symbol "exit", FunEntry {level=Translate.outermost, label=Temp.namedlabel("exit"), formals=[Types.INT], result=Types.UNIT})
                           ]
      fun insert ((symb, enventry), table) = Symbol.enter(table, symb, enventry)
    in
      foldl insert Symbol.empty baseFunList
    end
end
