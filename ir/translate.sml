signature TRANSLATE =
sig
    type level
    type access
    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
end

structure Translate =
struct
structure A = Absyn
structure T = Tree
structure F = MipsFrame
fun err(pos,message) = ErrorMsg.error pos message

datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm

datatype level = INNER of {frame: F.frame, parent: level, unique: unit ref}
               | OUTERMOST

val frags: F.frag list ref = ref []
(* Access to variables / functions in OUTERMOST level *)
exception InvalidAccess
exception Compiler
val debug = Utils.debug

fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
    in
        T.ESEQ(Utils.seq[T.MOVE(T.TEMP r, T.CONST 1),
                   genstm(t, f),
                   T.LABEL f,
                   T.MOVE(T.TEMP r, T.CONST 0),
                   T.LABEL t],
               T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

fun unCx (Cx f) = f
  | unCx (Ex e) = (fn(t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
  | unCx (Nx _) = (debug("Compiler error: Nx used as condition");
                   (fn(t, f) => T.LABEL(Temp.newlabel())))

fun unNx (Nx s) = s
  | unNx (Ex e) = T.EXP e
  | unNx (Cx f) =
    let val l = Temp.newlabel()
    in
        T.SEQ(f(l, l), T.LABEL l)
    end

(* Depth of a level w.r.t. the outermost (0th) level *)
fun depth OUTERMOST = 0
  | depth (INNER {frame, parent, unique}) = 1 + depth parent

type access = level * F.access

val outermost = OUTERMOST

(* need to add one true to represent static link, TODO but how to get the link *)
fun newLevel {parent=level, name=name, formals} =
    let
        val newFormals = true::formals
        val newFrame = F.newFrame {name=name, formals=newFormals}
    in
        INNER {frame=newFrame, parent=level, unique=ref ()}
    end

fun formals level =
    case level of OUTERMOST => []
                | INNER {frame, parent, unique} =>
                  foldl (fn(FrameAccess, curResult) => (level, FrameAccess)::curResult) [] (F.formals frame)

fun allocLocal level escape =
    case level of OUTERMOST =>
                  (err(0, "can't allocate Local varibale in the OUTERMOST level");
                   (OUTERMOST, (F.allocLocal (F.newFrame {name=Temp.newlabel(), formals=[]}) escape)))
                | INNER {frame, parent, unique} => (INNER {frame=frame, parent=parent, unique=unique}, (F.allocLocal frame escape))

fun followLink (curLevel, targetLevel) =
    case curLevel of OUTERMOST =>
                     (debug("Compiler error: followed link to OUTERMOST"); raise Compiler)
                   | INNER {frame=curFrame, parent=parent, unique=curUnique} =>
                     (case targetLevel of OUTERMOST =>
                                          (debug("Compiler error: OUTERMOST has no static link"); raise Compiler)
                                        | INNER {frame=targetFrame, parent=_, unique=targetUnique} =>
                                          if curUnique = targetUnique
                                          then T.MEM(T.TEMP F.FP)
                                          else T.MEM(followLink(parent, targetLevel)))
fun simpleVar ((decLevel: level, access: F.access), useLevel: level) =
    case decLevel of INNER {frame=decFrame, parent=_, unique=l} =>
                     (case useLevel of INNER {frame=useFrame, parent=parent, unique=r} =>
                                       if l = r
                                       then F.exp access (T.TEMP F.FP)
                                       else F.exp access (simpleVar((decLevel, access), parent))
                                     | _ => raise InvalidAccess)
                   | _ => raise InvalidAccess

fun transIf (conde, truee, falsee) =
    let val c = unCx conde
        and t = unEx truee
        and f = unEx falsee
        and lt = Temp.newlabel()
        and lf = Temp.newlabel()
        and ans = Temp.newtemp()
        and endlabel = Temp.newlabel()
    in
        Ex(T.ESEQ(Utils.seq[c(lt, lf),
                      T.LABEL lt,
                      T.MOVE(T.TEMP ans, t),
                      T.JUMP((T.NAME endlabel), [endlabel]),
                      T.LABEL lf,
                      T.MOVE(T.TEMP ans, f),
                      T.LABEL endlabel],
                  T.TEMP ans))
    end

fun transWhile (body, cond) =
    let val s = unNx body
        val c = unCx cond
        val l1 = Temp.newlabel()
        val l2 = Temp.newlabel()
    in
        Nx(Utils.seq[c(l1, l2),
               T.LABEL l1,
               s,
               c(l1, l2),
               T.LABEL l2])
    end

fun transFor (starte, ende, body) =
    let val r = Temp.newtemp()
        val rend = Temp.newtemp()
        val start' = unEx starte
        val end' = unEx ende
        val l1 = Temp.newlabel()
        val l2 = Temp.newlabel()
        val l3 = Temp.newlabel()
        val body' = unNx body
    in
        Nx(Utils.seq[T.MOVE (T.TEMP r, start'),
               T.MOVE (T.TEMP rend, end'),
               T.CJUMP (T.LE, T.TEMP r, T.TEMP rend, l1, l2),
               T.LABEL l3,
               T.EXP (T.BINOP (T.PLUS, T.TEMP r, T.CONST 1)),
               T.LABEL l1,
               body',
               T.CJUMP (T.LT, T.TEMP r, T.TEMP rend, l3, l2),
               T.LABEL l2])
    end

fun transInt (i) = T.CONST(i)

fun operToBinOp (A.PlusOp) = T.PLUS
  | operToBinOp (A.MinusOp) = T.MINUS
  | operToBinOp (A.TimesOp) = T.MUL
  | operToBinOp (A.DivideOp) = T.DIV
  | operToBinOp _ = (debug("Compiler error: invalid BinOp"); raise Compiler)
fun operToRelOp (A.LtOp) = T.LT
  | operToRelOp (A.LeOp) = T.LE
  | operToRelOp (A.GtOp) = T.GT
  | operToRelOp (A.GeOp) = T.GE
  | operToRelOp (A.EqOp) = T.EQ
  | operToRelOp (A.NeqOp) = T.NE
  | operToRelOp _ = (debug("Compiler error: invalid RelOp"); raise Compiler)
fun transBinOp (lefte, righte, oper) =
    let val left = unEx(lefte)
        val right = unEx(righte)
    in
        if (oper = A.PlusOp orelse
            oper = A.MinusOp orelse
            oper = A.TimesOp orelse
            oper = A.DivideOp)
        then Ex(T.BINOP(operToBinOp(oper), left, right))
        else
            let val tl = Temp.newlabel()
                val fl = Temp.newlabel()
                val endlabel = Temp.newlabel()
                val result = Temp.newtemp()
            in
                Ex(T.ESEQ(Utils.seq[T.CJUMP(operToRelOp(oper), left, right, tl, fl),
                              T.LABEL(tl),
                              T.MOVE(T.TEMP result, T.CONST 1),
                              T.JUMP(T.NAME endlabel, [endlabel]),
                              T.LABEL(fl),
                              T.MOVE(T.TEMP result, T.CONST 0),
                              T.LABEL(endlabel)],
                          T.TEMP result))
            end
    end
fun transSeqExp [] = Ex (T.CONST 0)
  | transSeqExp [exp] =
    let val exp = unEx exp
    in
        Ex (exp)
    end
  | transSeqExp (exp::rest) = Ex (T.ESEQ(unNx exp, unEx (transSeqExp (rest))))

fun transAssign (lvalue, rvalue) = Nx(T.MOVE(unEx lvalue, unEx rvalue))

fun transCall (curLevel, label, funcLevel, formals) =
    (* Functions in OUTERMOST do not take static links. *)
    case funcLevel of OUTERMOST => Ex(T.CALL(T.NAME label, map unEx formals))
                    | INNER {frame=decFrame, parent=_, unique=decUnique} =>
                      (case curLevel of INNER {frame=curFrame, parent=parent, unique=curUnique} =>
                                        (* If in the same level or self-recursion *)
                                        if curUnique = decUnique orelse (#label curFrame) = (#label decFrame)
                                        then Ex(T.CALL(T.NAME label, (T.MEM(T.TEMP F.FP)::(map unEx formals))))
                                        else if depth(curLevel) > depth(funcLevel)
                                        then Ex(T.CALL(T.NAME label, (followLink(curLevel, funcLevel)::(map unEx formals))))
                                        else Ex(T.CALL(T.NAME label, ((T.TEMP F.FP)::(map unEx formals))))
                                      | OUTERMOST => (debug("Compiler error: attempting to call in OUTERMOST"); raise Compiler))

fun genMove offset from (exp, moves) =
    let val curOffset = 4 * (length moves) + offset
    in
        (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(curOffset), from)), unEx exp))::moves
    end
fun transRecord (decFields, actualFields) =
    let val addr = Temp.newtemp()
        val len = length(decFields)
        val genMove = genMove 0 (T.TEMP F.RV)
        val moves = if (length actualFields) = 0
                    then foldr genMove [] (Utils.arrayMul(Ex(T.CONST 0), len))
                    else foldr genMove [] actualFields
    in
        Ex(T.ESEQ(Utils.seq([T.EXP(F.externalCall("malloc", [T.CONST(4 * len)])),
                       T.MOVE(T.TEMP addr, T.TEMP F.RV)]
                      @ moves),
                  T.TEMP addr))
    end

fun transArray (initExp: exp, len) =
    let val addr = Temp.newtemp()
        val actualAddr = Temp.newtemp()
        val genMove = genMove 0 (T.TEMP actualAddr)
        val moves = foldr genMove [] (Utils.arrayMul(initExp, len))
    in
        Ex(T.ESEQ(Utils.seq([T.EXP(F.externalCall("malloc", [T.CONST(4 * (len + 1))])),
                       T.MOVE(T.TEMP addr, T.TEMP F.RV),
                       T.MOVE(T.TEMP actualAddr, T.BINOP(T.PLUS, T.TEMP addr, T.CONST(4)))]
                      @ moves
                      @ [T.MOVE(T.TEMP addr, T.CONST(len))]),
                  T.TEMP actualAddr))
    end

fun procEntryExit (OUTERMOST, body) = (debug("Compiler error: Function declaration in OUTERMOST"); raise Compiler)
  | procEntryExit (INNER {frame, parent, unique}, body) =
    let
        val proc = F.procEntryExit1(frame, unEx body)
        val _ = frags := (proc::(!frags))
    in
        ()
    end

end
