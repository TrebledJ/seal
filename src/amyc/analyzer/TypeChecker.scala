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
      ???
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: TypeOrVar)(implicit env: Map[Identifier, TypeOrVar]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: TypeOrVar): List[Constraint] =
        ???
      
      e match {
        case _ => ???
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
        case _ => ???
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
