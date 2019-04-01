structure FindEscape : sig
              val findEscape: Absyn.exp -> unit
          end =
struct

structure A = Absyn
structure S = Symbol

type depth = int
type escEnv = (depth * bool ref) S.table

fun traverseVar (env: escEnv, d: depth, s: A.var): unit =
    case s of A.SimpleVar (symbol, pos) =>
              (case S.look(env, symbol)
                of SOME(decDepth, r) =>
                   if decDepth < d
                   then (r := true;
                         Utils.debug("Variable "
                                     ^ S.name symbol
                                     ^ " escapes at pos "
                                     ^ Int.toString pos))
                   else ()
                 | NONE => Utils.warning("Variable "
                                         ^ S.name symbol
                                         ^ " was never declared."))
            | A.FieldVar (var, _, _) => traverseVar(env, d, var)
            | A.SubscriptVar (var, _, _) => traverseVar(env, d, var)
and traverseExp (env: escEnv, d: depth, s: A.exp): unit =
    case s of A.VarExp(var) => traverseVar(env, d, var)
            | A.OpExp {left, oper, right, pos} => (traverseExp(env, d, left);
                                                   traverseExp(env, d, right))
            | A.LetExp {decs, body, pos} =>
              let val env' = traverseDecs(env, d, decs)
              in
                  traverseExp(env', d, body)
              end
            | A.SeqExp exps => (map (fn (exp) => traverseExp(env, d, exp)) (map #1 exps);
                                ())
            | A.AssignExp {var, exp, pos} => (traverseVar(env, d, var);
                                              traverseExp(env, d, exp))
            | A.IfExp {test, then', else', pos} =>
              (traverseExp(env, d, test);
               traverseExp(env, d, then');
               case else' of SOME(exp) => traverseExp(env, d, exp)
                           | NONE => ())
            | A.CallExp {args=args, ...} => (map (fn (exp) => traverseExp(env, d, exp)) args;
                                             ())
            | A.RecordExp {fields=fields, ...} =>
              (map (fn (exp) => traverseExp(env, d, exp)) (map #2 fields);
               ())
            | A.WhileExp {test, body, pos} => (traverseExp(env, d, test);
                                               traverseExp(env, d, body))
            | A.ArrayExp {size=size, init=init, ...} => (traverseExp(env, d, size);
                                                         traverseExp(env, d, init))
            | A.ForExp {var, escape, lo, hi, body, pos} =>
              let val env' = S.enter(env, var, (d, escape))
              in
                  traverseExp(env, d, lo);
                  traverseExp(env, d, hi);
                  traverseExp(env', d, body)
              end
            | A.NilExp => ()
            | A.IntExp _ => ()
            | A.StringExp _ => ()
            | A.BreakExp _ => ()
and traverseDecs (env: escEnv, d: depth, s: A.dec list): escEnv =
    let fun traverseDec (dec: A.dec, env: escEnv): escEnv =
            case dec
             of A.VarDec {name, escape, typ, init, pos} =>
                (* Enter the escape on absyn tree to be modified if *)
                (* variable escapes *)
                S.enter(env, name, (d, escape))
              | A.FunctionDec decs =>
                let fun traverseFuncDec ({name, params, result, body, pos}) =
                        let val _ = Utils.debug("Entering declaration for function " ^ S.name name)
                            fun enterParam ({name, escape, typ, pos}, env) =
                                S.enter(env, name, (d, escape))
                            (* Create new environment with params *)
                            val env' = foldl enterParam env params
                            (* And apply it to the body at depth d + 1 *)
                            val _ = traverseExp(env', d + 1, body)
                        in
                            (* Params do not carry over to the next function *)
                            (* So we don't need to return a new environtment *)
                            ()
                        end
                    val _ = map traverseFuncDec decs
                in
                    env
                end
              (* Do nothing for type decs *)
              | A.TypeDec _ => env
    in
        foldl traverseDec env s
    end

fun findEscape(prog: Absyn.exp): unit = traverseExp(S.empty, 0, prog)

end
