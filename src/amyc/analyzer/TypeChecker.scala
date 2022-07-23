package amyc
package analyzer

import scala.language.implicitConversions
import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: TypeOrVar, expected: TypeOrVar, pos: Position)

    type TypeOrVar = ConcreteType | TypeVariable

    // Represents a type variable.
    // It is meant only for internal type checker use,
    // since no Amy value can have such type.
    case class TypeVariable private (id: Int)
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Proxy ADT that can handle type variables in tuple and function types.
    sealed trait ConcreteType
    sealed trait IntCType extends ConcreteType
    case object IntAnyValueType extends IntCType
    case class IntConstantType(n: Int) extends IntCType // Intended to work like C++'s constexpr int.
    case object BooleanCType extends ConcreteType
    case object StringCType extends ConcreteType
    case object UnitCType extends ConcreteType
    case object ErrorCType extends ConcreteType // Band-aid to ensure everything resolves to ctype. // TODO: remove when adding generics
    case class ClassCType(qname: QualifiedName) extends ConcreteType
    case class TupleCType(xs: List[TypeOrVar]) extends ConcreteType
    case class FunctionCType(from: List[TypeOrVar], to: TypeOrVar) extends ConcreteType
    
    implicit def typeToCType(t: Type): ConcreteType = {
      // Implicit converter.
      t match {
        case IntType => IntAnyValueType
        case BooleanType => BooleanCType
        case StringType => StringCType
        case UnitType => UnitCType
        case ClassType(qname) => ClassCType(qname)
        case TupleType(ts) => TupleCType(ts map (_.tpe) map (typeToCType))
        case FunctionType(from, to) => FunctionCType(from map (t => typeToCType(t.tpe)), to.tpe)
      }
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: TypeOrVar)(implicit env: Map[Identifier, TypeOrVar]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: TypeOrVar): List[Constraint] =
        List(Constraint(found, expected, e.position))

      // For binary operations of type (T, T) => U.
      def binOpConstraintWithRet(lhs: Expr, rhs: Expr, coerceType: TypeOrVar, returnType: TypeOrVar): List[Constraint] =
        genConstraints(lhs, coerceType) ++ genConstraints(rhs, coerceType)
            ++ topLevelConstraint(returnType)
        
      // For binary operations of type (T, T) => T.
      def binOpConstraint(lhs: Expr, rhs: Expr, coerceType: TypeOrVar): List[Constraint] =
        binOpConstraintWithRet(lhs, rhs, coerceType, coerceType)

      def unaryOpConstraint(expr: Expr, coerceType: TypeOrVar): List[Constraint] =
        genConstraints(expr, coerceType) ++ topLevelConstraint(coerceType)
      
      e match {
        case IntLiteral(n) => topLevelConstraint(IntConstantType(n))
        case BooleanLiteral(_) => topLevelConstraint(BooleanCType)
        case StringLiteral(_) => topLevelConstraint(StringCType)
        case UnitLiteral() => topLevelConstraint(UnitCType)
        
        case Variable(name) => 
          // Lookup variable in environment. Since Name Analysis has been run, assume that name is typed.
          val found = env.getOrElse(name, {
            // Look up in symbol table.
            table.getConstructor(name).orElse(table.getFunction(name)) match {
              // Construct function type from signature.
              case Some(sig) =>
                val paramTypes = sig.argTypes map typeToCType
                FunctionCType(paramTypes, sig.retType)
              case None =>
                fatal(s"Variable, function, or constructor $name not found. How did we get here? NAME ANALYSER -- YOU HAD *ONE* JOB.", e)
            }
          })
          topLevelConstraint(found)

        case Tuple(exprs) =>
          // Generate variables for each field. Bind variables to found + expected fields.
          val vars = exprs map (_ => TypeVariable.fresh())
          ((exprs zip vars) flatMap genConstraints)
            ++ topLevelConstraint(TupleCType(vars))

        case Lambda(params, retType, body) =>
          val retTypeOrVar: TypeOrVar = retType match {
            case None => TypeVariable.fresh()
            case Some(t) => t.tpe
          }
          val paramTypes = params map (p => typeToCType(p.tt.tpe))
          val found = FunctionCType(paramTypes, retTypeOrVar)
          genConstraints(body, retTypeOrVar)(env ++ (params map (p => p.name -> typeToCType(p.tt.tpe))))
            ++ topLevelConstraint(found)

        case Plus(lhs, rhs) => binOpConstraint(lhs, rhs, IntAnyValueType)
        case Minus(lhs, rhs) => binOpConstraint(lhs, rhs, IntAnyValueType)
        case Times(lhs, rhs) => binOpConstraint(lhs, rhs, IntAnyValueType)
        case Div(lhs, rhs) => binOpConstraint(lhs, rhs, IntAnyValueType)
        case Mod(lhs, rhs) => binOpConstraint(lhs, rhs, IntAnyValueType)
        case LessThan(lhs, rhs) => binOpConstraintWithRet(lhs, rhs, IntAnyValueType, BooleanCType)
        case LessEquals(lhs, rhs) => binOpConstraintWithRet(lhs, rhs, IntAnyValueType, BooleanCType)
        case And(lhs, rhs) => binOpConstraint(lhs, rhs, BooleanCType)
        case Or(lhs, rhs) => binOpConstraint(lhs, rhs, BooleanCType)
        case Equals(lhs, rhs) => binOpConstraintWithRet(lhs, rhs, TypeVariable.fresh(), BooleanCType)
        case Concat(lhs, rhs) => binOpConstraint(lhs, rhs, StringCType)
          
        case Not(e) => unaryOpConstraint(e, BooleanCType)
        case Neg(e) => unaryOpConstraint(e, IntAnyValueType)

        case Call(expr, args) =>
          val typeVars = args map (_ => TypeVariable.fresh())
          val funcType = FunctionCType(typeVars, expected)
          ((args zip typeVars) flatMap genConstraints) // Solve argument constraints first.
            ++ genConstraints(expr, funcType)

        case Sequence(e1, e2) =>
          genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, expected)

        case Let(df, value, body) =>
          // Unlike Sequence, Let modifies the type environment.
          genConstraints(value, df.tt.tpe) // Check value expression type checks with annotation.
            ++ genConstraints(body, expected)(env + (df.name -> df.tt.tpe)) // Assume let-stmt type checks and type check the rest of the body using the expected type.
          
        case Ite(cond, thenn, elze) =>
          // `cond` should type check to Bool.
          // `thenn` and `elze` should type check to T.
          genConstraints(cond, BooleanCType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: TypeOrVar):
            (List[Constraint], Map[Identifier, TypeOrVar]) =
          {
            pat match {
              case WildcardPattern() => (List(), Map()) // No constraints and bindings.
              case IdPattern(name) => (List(), Map(name -> scrutExpected)) // Bind name to expected type.
              case LiteralPattern(lit) =>
                // ctx.reporter.info(s"Encountered literal pattern ${lit}")
                val cst = lit match {
                  case IntLiteral(_) => Constraint(IntAnyValueType, scrutExpected, pat.position) // Int value of patterns not checked here.
                  case BooleanLiteral(_) => Constraint(BooleanCType, scrutExpected, pat.position)
                  case StringLiteral(_) => Constraint(StringCType, scrutExpected, pat.position)
                  case UnitLiteral() => Constraint(UnitCType, scrutExpected, pat.position)
                }
                (List(cst), Map())
              case CaseClassPattern(constr, args) =>
                // Recurse and bind args to constructor signature in symbol table.
                val sig = table.getConstructor(constr).get
                val typ = sig.parent
                val cst = Constraint(ClassCType(typ), scrutExpected, pat.position)

                val subpatterns = args.zip(sig.argTypes).map(handlePattern(_, _))
                val cs = cst :: subpatterns.map(_._1).flatten
                val bs = if subpatterns.isEmpty then Map() else subpatterns.map(_._2).reduce(_ ++ _)
                (cs, bs)
              case TuplePattern(args) =>
                // We know the length of the tuple. Use type variables and recurse them as scrut.
                val typeVars = args map (_ => TypeVariable.fresh())

                // Bind type vars to pattern.
                val subpatterns = args.zip(typeVars).map(handlePattern(_, _))

                // Bind type vars as tuple with scrut.
                val patCType = TupleCType(typeVars)
                val scrutCons = Constraint(patCType, scrutExpected, pat.position)

                val (cs, bs) = subpatterns.unzip
                (scrutCons :: cs.flatten, if bs.isEmpty then Map() else bs.reduce(_ ++ _))
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: TypeOrVar): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))
  
        case Error(e) =>
          topLevelConstraint(ErrorCType)
            ++ genConstraints(e, StringCType)
      }
    }

    
    // Finds all the type vars in constraints.
    def findTypeVars(constraints: List[Constraint]): List[TypeVariable] = {
      def find(tpe: TypeOrVar): List[TypeVariable] = {
        tpe match {
          case t@TypeVariable(_) => List(t)
          case TupleCType(xs) => xs flatMap find
          case FunctionCType(args, ret) => (args flatMap find) ++ find(ret)
          case _ => List()
        }
      }

      constraints flatMap {
        case Constraint(found, expected, _) =>
          find(found) ++ find(expected)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: TypeOrVar): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: TypeOrVar, from: Int, to: TypeOrVar): TypeOrVar = {
        tpe match {
          case TypeVariable(`from`) => to
          
          // Recursively substitute into compound types.
          case FunctionCType(tFrom, tTo) => FunctionCType(tFrom map (subst(_, from, to)), subst(tTo, from, to))
          case TupleCType(ts) => TupleCType(ts map (subst(_, from, to)))
          case _ => tpe
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }


    // Solves constraints and checks if all type variables are resolved.
    // Ensures that all variables can be resolved to fully concrete type.
    // (Fully concrete meaning that child types are also concrete, for compound types.)
    def solveConstraintsFull(constraints: List[Constraint]): Unit = {
      // TODO: If extending with generics, play around here and add a secondary "case GenericType(var) extends ConcreteType".
      def check(t: TypeOrVar)(implicit map: Map[TypeVariable, ConcreteType]): Unit = {
        t match {
          case v@TypeVariable(_) =>
            // Check if each type variable has been resolved.
            map.get(v) match {
              case None => error(s"type variable unresolved: $v")
    
              // We're not done yet! Check type variables in compound types are resolved.
              case Some(TupleCType(xs)) => xs map check
              case Some(FunctionCType(args, ret)) => args map check; check(ret)
    
              case Some(_) => ; // OK, resolved.
            }
          case _ => ; // OK, it wasn't a type variable.
        }
      }

      val typeVars = findTypeVars(constraints)
      val resultMap = solveConstraints(constraints)(Map())
      typeVars foreach (check(_)(resultMap))
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint], iter: Int = 1)
                        (implicit varMap: Map[TypeVariable, ConcreteType]): Map[TypeVariable, ConcreteType] = {
      // if (iter == 1)
      //   ctx.reporter.info(s"---- Pass #${iter} ---- (${constraints.length} constraints)")
      //   constraints.foreach(x => ctx.reporter.info(s"    ${x}"))

      constraints match {
        case Nil => varMap
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found, expected) match {
            // Matching concrete types.
            case (ft: ConcreteType, et: ConcreteType) if ft == et => solveConstraints(more, iter+1)
            case (it: IntCType, jt: IntCType) => solveConstraints(more, iter+1) // OK. Equality checking is done in execution.

            // Break down tuple/function types.
            case (FunctionCType(ffrom, fto), FunctionCType(efrom, eto)) =>
              // TODO: report "higher level" mistakes for function types
              if (ffrom.length != efrom.length) {
                error(s"expected function that accepts ${efrom.length} arguments, got function that accepts ${ffrom.length} arguments", pos)
              }
              solveConstraints(((ffrom zip efrom) map (Constraint(_, _, pos))) ++ (Constraint(fto, eto, pos)::more), iter+1)

            case (TupleCType(ts1), TupleCType(ts2)) =>
              if (ts1.length != ts2.length) {
                error(s"expected tuple of length ${ts2.length}, got tuple of length ${ts1.length}", pos)
                varMap
              } else {
                val argConstraints = (ts1 zip ts2) map (Constraint(_, _, pos))
                solveConstraints(argConstraints ++ more, iter+1)
              }

            // Static checking on tuple access.
            case (TupleCType(ts), FunctionCType(args, expected)) =>
              if (args.length != 1) {
                error(s"tried calling ${args.length} arguments on a tuple; only 1 argument calls are allowed on tuples", pos)
                varMap
              } else {
                args.head match {
                  case IntConstantType(i) =>
                    if (i >= ts.length) {
                      error(s"tuple access out of bounds; tuple has length ${ts.length}, can't access index $i", pos)
                      varMap
                    } else {
                      solveConstraints(Constraint(ts(i), expected, pos)::more, iter+1)
                    }
                  case _ =>
                    error(s"expected integer constant in tuple access, got $args", pos)
                    varMap
                }
              }

            // Redundant constraint: same type variables.
            case (ft: TypeVariable, et: TypeVariable) if ft.id == et.id => solveConstraints(more, iter+1)

            // Substitute type variables for concrete types.
            case (ft: ConcreteType, et: TypeVariable) =>
              solveConstraints(subst_*(constraints, et.id, ft), iter+1)(varMap + (et -> ft))
            case (ft: TypeVariable, et: ConcreteType) =>
              solveConstraints(subst_*(constraints, ft.id, et), iter+1)(varMap + (ft -> et))

            case (a: TypeVariable, b: TypeVariable) => 
              // Replace all occurrences of var b with var a.
              solveConstraints(subst_*(constraints, b.id, a), iter+1)

            // Ignore error ctype.
            case (ErrorCType, _) => solveConstraints(more, iter+1)

            // Other cases (clashing).
            case (ft, et) =>
              error(s"expected ${et}, got ${ft}", pos)
              varMap
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env: Map[Identifier, TypeOrVar] = params.map{ case ParamDef(name, tt) => name -> typeToCType(tt.tpe) }.toMap
        solveConstraintsFull(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraintsFull(genConstraints(e, tv)(Map())))
    }

    v

  }
}
