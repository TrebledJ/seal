package amyc
package interpreter

import scala.util.Random
import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable

import java.util.Scanner
import java.io.FileReader

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.mkString(", ") + ")"
      case TupleValue(args) =>
        "(" + args.mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value
  case class TupleValue(xs: List[Value]) extends Value
  case class FileReaderValue(sc: Scanner) extends Value

  sealed trait FunctionValue extends Value
  case class BuiltInFunctionValue(func: List[Value] => Value) extends FunctionValue
  case class FunctionPtrValue(funcDef: FunDef) extends FunctionValue
  case class ConstructorPtrValue(qname: Identifier) extends FunctionValue
  case class ClosureValue(proc: Lambda, env: Map[Identifier, Value]) extends FunctionValue

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    def checkFileEOF(sc: Scanner, attempt: String) = {
      if (!sc.hasNext)
        ctx.reporter.fatal(s"encountered EOF while trying to read Int")
    }

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) },
      ("File", "open") -> { args => FileReaderValue(Scanner(FileReader(args.head.asString))) },
      ("File", "isEOF") -> {
        case FileReaderValue(sc)::_ => BooleanValue(!sc.hasNext)
        case _ => ctx.reporter.fatal("expected file")
      },
      ("File", "readLine") -> {
        case FileReaderValue(sc)::_ => checkFileEOF(sc, "String"); StringValue(sc.nextLine)
        case _ => ctx.reporter.fatal("expected file")
      },
      ("File", "readInt") -> {
        case FileReaderValue(sc)::_ => checkFileEOF(sc, "Int"); IntValue(sc.nextInt)
        case _ => ctx.reporter.fatal("expected file")
      },
      ("File", "readString") -> {
        case FileReaderValue(sc)::_ => checkFileEOF(sc, "String"); StringValue(sc.next)
        case _ => ctx.reporter.fatal("expected file")
      },
      ("File", "readBoolean") -> {
        case FileReaderValue(sc)::_ => checkFileEOF(sc, "Boolean"); BooleanValue(sc.nextBoolean)
        case _ => ctx.reporter.fatal("expected file")
      },
      ("Str", "equals") -> { args => BooleanValue(args(0).asString == args(1).asString) },
      ("Random", "int") -> {
        args => {
          val n = args(0).asInt;
          if (n < 0) {
            ctx.reporter.fatal(s"expected non-negative number, got $n")
          }
          IntValue(new Random().nextInt(n))
        }
      },
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier): Boolean = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier): String = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String): FunDef = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(qname) =>
          if (locals.contains(qname)) {
            locals(qname)
          } else if (isConstructor(qname)) {
            ConstructorPtrValue(qname)
          } else {
            // Otherwise it's a function.
            val owner = findFunctionOwner(qname)
            if (builtIns contains (owner, qname.name)) {
              BuiltInFunctionValue(builtIns((owner, qname.name)))
            } else {
              FunctionPtrValue(findFunction(owner, qname.name))
            }
          }

        case IntLiteral(i) => IntValue(i)
        case BooleanLiteral(b) => BooleanValue(b)
        case StringLiteral(s) => StringValue(s)
        case UnitLiteral() => UnitValue
        case Tuple(xs) => TupleValue(xs map interpret)
        case proc@Lambda(params, _, body) => ClosureValue(proc, locals)

        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) =>
          val rval = interpret(rhs).asInt
          if (rval == 0)
            ctx.reporter.fatal(s"division by 0", expr)
          else
            IntValue(interpret(lhs).asInt / rval)
            
        case Mod(lhs, rhs) =>
          val rval = interpret(rhs).asInt
          if (rval == 0)
            ctx.reporter.fatal(s"division by 0", expr)
          else
            IntValue(interpret(lhs).asInt % rval)

        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)

        case Equals(lhs, rhs) =>
          def valueEquals(val1: Value, val2: Value): Boolean = {
            // Int, Bool, Unit: ValueEquality.
            (val1, val2) match
              case (IntValue(val1), IntValue(val2)) => val1 == val2
              case (BooleanValue(val1), BooleanValue(val2)) => val1 == val2
              case (UnitValue, UnitValue) => true // Well duh!

              // Check ReferenceEquality types: String, user-defined types.
              case (StringValue(_), StringValue(_)) | (CaseClassValue(_, _), CaseClassValue(_, _)) =>
                val1 eq val2

              // Memberwise equality for tuples.
              case (TupleValue(xs), TupleValue(ys)) =>
                (xs zip ys).map((x, y) => valueEquals(x, y)).forall(_ == true)

              case (_, _) => false // Everything else.
          }
          
          BooleanValue(valueEquals(interpret(lhs), interpret(rhs)))

        case Concat(lhs, rhs) => StringValue(interpret(lhs).asString + interpret(rhs).asString)
        case Not(e) => BooleanValue(!interpret(e).asBoolean)
        case Neg(e) => IntValue(-interpret(e).asInt)

        case Call(e, args) =>
          val argValues = args.map(interpret)
          val value = interpret(e)
          value match {
            case TupleValue(xs) => xs(argValues(0).asInt)
            case BuiltInFunctionValue(func) => func(argValues)
            case FunctionPtrValue(funcDef) =>
              // Construct a map of locals within function scope.
              val funcLocals: Map[Identifier, Value] = funcDef.params.map(_.name).zip(argValues).toMap;
              interpret(funcDef.body)(funcLocals)
            case ConstructorPtrValue(qname) => CaseClassValue(qname, argValues)
            case ClosureValue(proc, env) =>
              interpret(proc.body)(env ++ (proc.params zip argValues).map((p, v) => p.name -> v))
            case _ => ctx.reporter.fatal("Typechecker-- Y U no work?!?", expr)
          }

        case Sequence(e1, e2) => interpret(e1); interpret(e2)

        case Let(df, value, body) =>
          interpret(body)(locals + (df.name -> interpret(value))) // Pass on an updated map.

        case Ite(cond, thenn, elze) =>
          if (interpret(cond).asBoolean)
            interpret(thenn)
          else
            interpret(elze)

        case Match(scrut, cases) =>
          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) => Some(List()) // Matched, no binding needed.
              case (_, IdPattern(name)) => Some(List(name -> v))

              // Check if literals match with values.
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) => if (i1 == i2) Some(List()) else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) => if (b1 == b2) Some(List()) else None

              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                None // Strings should never match according to spec (ReferenceEquality).

              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(List()) // The duh case... Duh duh case? Duh the case.

              case (CaseClassValue(valId, values), CaseClassPattern(patId, patterns)) =>
                if (valId != patId) // Constructor no matchy.
                  None
                else
                  // Recursively pattern match.
                  values.zip(patterns).map(matchesPattern(_, _))
                    .fold(Some(List()))
                      ((acc, maybexs) => 
                        maybexs.flatMap(xs => 
                          acc.flatMap(ys => Some(xs ++ ys))))

              case (TupleValue(xs), TuplePattern(ps)) => 
                val res = ((xs zip ps) map matchesPattern)
                if (res.exists(_ == None))
                  None
                else
                  Some(res.flatten.flatten)
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) =>
          ctx.reporter.fatal(interpret(msg).asString, expr)
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(Map())
    }
  }
}
