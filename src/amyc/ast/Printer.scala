package amyc.ast

import scala.language.implicitConversions
import amyc.utils._

// A printer for Amy trees
trait Printer {

  val treeModule: TreeModule
  import treeModule._

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document
  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document

  protected implicit def stringToDoc(s: String): Raw = Raw(s)

  def apply(t: Tree)(implicit printUniqueIDs: Boolean = false): String = {

    def binOp(e1: Expr, op: String, e2: Expr) = "(" <:> rec(e1) <:> " " + op + " " <:> rec(e2) <:> ")"

    def rec(t: Tree, parens: Boolean = true): Document = t match {
      /* Definitions */
      case Program(modules) =>
        Stacked(modules map (rec(_)), emptyLines = true)

      case ModuleDef(name, defs, optExpr) =>
        stack(
          "object " <:> name <:> " {",
          "",
          Indented(Stacked(defs ++ optExpr.toList map (rec(_, false)), emptyLines = true)),
          "}",
          ""
        )

      case AbstractClassDef(name) =>
        "abstract class " <:> printName(name)

      case CaseClassDef(name, fields, parent) =>
        def printField(f: TypeTree) = "v: " <:> rec(f)
        "case class " <:> name <:> "(" <:> Lined(fields map printField, ", ") <:> ") extends " <:> parent

      case FunDef(name, params, retType, body) =>
        stack(
          "def " <:> name <:> "(" <:> Lined(params map (rec(_)), ", ") <:> "): " <:> rec(retType) <:> " = {",
          Indented(rec(body, false)),
          "}"
        )

      case ParamDef(name, tpe) =>
        name <:> ": " <:> rec(tpe)

      /* Expressions */
      case Variable(name) =>
        name
      case IntLiteral(value) =>
        value.toString
      case BooleanLiteral(value) =>
        value.toString
      case StringLiteral(value) =>
        "\"" + value + '"'
      case UnitLiteral() =>
        "()"
      case Tuple(xs) =>
        "(" <:> Lined(xs.map(rec(_)), ", ") <:> ")"
      case Lambda(params, retType, body) =>
        val retTypeRec: Document = retType match {
          case None => ""
          case Some(t) => ": " <:> rec(t)
        }
        "\\" <:> "(" <:> Lined(params.map(rec(_)), ", ") <:> ")" <:> retTypeRec <:> " -> " <:> rec(body)
      case Plus(lhs, rhs) =>
        binOp(lhs, "+", rhs)
      case Minus(lhs, rhs) =>
        binOp(lhs, "-", rhs)
      case Times(lhs, rhs) =>
        binOp(lhs, "*", rhs)
      case Div(lhs, rhs) =>
        binOp(lhs, "/", rhs)
      case Mod(lhs, rhs) =>
        binOp(lhs, "%", rhs)
      case LessThan(lhs, rhs) =>
        binOp(lhs, "<", rhs)
      case LessEquals(lhs, rhs) =>
        binOp(lhs, "<=", rhs)
      case And(lhs, rhs) =>
        binOp(lhs, "&&", rhs)
      case Or(lhs, rhs) =>
        binOp(lhs, "||", rhs)
      case Equals(lhs, rhs) =>
        binOp(lhs, "==", rhs)
      case Concat(lhs, rhs) =>
        binOp(lhs, "++", rhs)
      case Not(e) =>
        "!(" <:> rec(e) <:> ")"
      case Neg(e) =>
        "-(" <:> rec(e) <:> ")"
      case Call(expr, args) =>
        rec(expr) <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"
      case Sequence(lhs, rhs) =>
        val main = stack(
          rec(lhs, false) <:> ";",
          rec(rhs, false),
        )
        if (parens) {
          stack(
            "(",
            Indented(main),
            ")"
          )
        } else {
          main
        }
      case Let(df, value, body) =>
        val main = stack(
          "val " <:> rec(df) <:> " =",
          Indented(rec(value)) <:> ";",
          rec(body, false) // For demonstration purposes, the scope or df is indented
        )
        if (parens) {
          stack(
            "(",
            Indented(main),
            ")"
          )
        } else {
          main
        }
      case Ite(cond, thenn, elze) =>
        stack(
          "(if(" <:> rec(cond) <:> ") {",
          Indented(rec(thenn)),
          "} else {",
          Indented(rec(elze)),
          "})"
        )
      case Match(scrut, cases) =>
        stack(
          rec(scrut) <:> " match {",
          Indented(Stacked(cases map (rec(_)))),
          "}"
        )
      case Error(msg) =>
        "error(" <:> rec(msg) <:> ")"

      /* cases and patterns */
      case MatchCase(pat, expr) =>
        stack(
          "case " <:> rec(pat) <:> " =>",
          Indented(rec(expr))
        )
      case WildcardPattern() =>
        "_"
      case IdPattern(name) =>
        name
      case LiteralPattern(lit) =>
        rec(lit)
      case CaseClassPattern(name, args) =>
        name <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"
      case TuplePattern(args) =>
        "(" <:> Lined(args map (rec(_)), ", ") <:> ")"

      /* Types */
      case TypeTree(tp) =>
        tp match {
          case IntType => "Int"
          case BooleanType => "Boolean"
          case StringType => "String"
          case UnitType => "Unit"
          case ClassType(name) => name
          case TupleType(_) => tp.toString
          case FunctionType(_, _) => tp.toString
        }

    }

    rec(t).print
  }
}

object NominalPrinter extends Printer {
  val treeModule: NominalTreeModule.type = NominalTreeModule
  import NominalTreeModule._

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = Raw(name)

  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document = {
    Raw(name match {
      case QualifiedName(Some(module), name) =>
        s"$module.$name"
      case QualifiedName(None, name) =>
        name
    })
  }
}

object SymbolicPrinter extends SymbolicPrinter
trait SymbolicPrinter extends Printer {
  val treeModule: SymbolicTreeModule.type = SymbolicTreeModule
  import SymbolicTreeModule._

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = {
    if (printUniqueIds) {
      name.fullName
    } else {
      name.name
    }
  }

  @inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document = {
    printName(name)
  }
}
