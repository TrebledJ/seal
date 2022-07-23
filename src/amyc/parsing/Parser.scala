package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Token._
import TokenKind._

import scallion._

/**
 * 
 * LL(1) Grammar for Amy Expressions:
 * 
 * E0 ::= E1 | val ParamDef = E1; E0
 * E1 ::= E2 E1Rest | if (E0) { E0 } else { E0 }
 * E2 ::= E3 | E2 BinOp E3
 * E3 ::= UnaryOp E4
 * E4 ::= error(E0)
 *     | Id IdRest
 *     | ( bracketedExpr
 *     | OtherLiteral
 * 
 * E1Rest ::= epsilon | Match+
 * Match ::= match { MatchCase+ }
 * 
 * // Handles Id and Function calling
 * IdRest ::= epsilon
 *     | [.Id]? ( Args )
 * 
 * bracketedExpr ::= )
 *     | E0[,E0]* )
 * 
 */

/**
 * 
 * Note [Tuple Calls]
 * ==================
 * 
 * In Scala, function types work a bit weird with tuples.
 * 
 * def f(x: Int, y: Int): Int = x + y
 * def g(a: (Int, Int)): Int = a(0) + a(1)
 * 
 * f : (Int, Int) => Int
 * g : ((Int, Int)) => Int
 * 
 * f(1, 2) // OK.
 * f((1, 2)) // Error: found (Int, Int), required Int.
 * g(1, 2) // OK!?
 * g((1, 2)) // OK.
 * 
 * val h: (Int, Int) => Int = f; // OK.
 * val h: (Int, Int) => Int = g; // Error: found ((Int, Int)) => Int.
 * val h: ((Int, Int)) => Int = f; // OK!?
 * val h: ((Int, Int)) => Int = g; // OK.
 * 
 * The issue with parsing is in the function taking a tuple argument. The syntax
 * is ((Int, Int)), i.e. two pairs of parentheses. This seems to be unique for
 * function arguments. Outside the context of a function argument, the
 * parentheses are redundant.
 * 
 * Some examples:
 * 
 *  - (Int)                 -> Int
 *  - (Int => Int)          -> Int => Int
 *  - ((Int, Int))          -> (Int, Int)
 *  - ((Int, Int)) => Int   -> ((Int, Int)) => Int
 *  - ((Int, Int)) => (Int) -> ((Int, Int)) => Int
 * 
 * Note that for multiple arguments where the first argument is a tuple, it
 * is clear that the outer set of parentheses are *not* redundant.
 * 
 *  - ((Int, Int), String) => Int
 * 
 */


// The parser for Amy
// See Scallion docs: https://epfl-lara.github.io/scallion/scallion/index.html
// Notes on Scallion: https://lptk.github.io/clpcd/labs/lab3-scallion.html
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }
  def opPos(string: String): Syntax[(String, Position)] = accept(OperatorKind(string)) { case tok@OperatorToken(name) => (name, tok.position) }
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
    case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  lazy val abstractClassDef: Syntax[AbstractClassDef] = (kw("abstract") ~ kw("class") ~ identifier).map {
    case kw ~ _ ~ id => AbstractClassDef(id).setPos(kw)
  }

  lazy val caseClassDef: Syntax[CaseClassDef] = (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map {
    case kw ~ _ ~ name ~ _ ~ params ~ _ ~ _ ~ parent => CaseClassDef(name, params.map(_.tt), parent).setPos(kw)
  }

  lazy val funDef: Syntax[FunDef] = (kw("def") ~ identifier ~<~ "(" ~ parameters ~<~ ")" ~<~ ":" ~ typeTree ~<~ "=" ~<~ "{" ~ expr ~<~ "}").map {
    case d ~ name ~ params ~ retType ~ body => FunDef(name, params, retType, body).setPos(d)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] = abstractClassDef.up[ClassOrFunDef] | caseClassDef.up[ClassOrFunDef] | funDef.up[ClassOrFunDef]

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] = (identifier ~<~ ":" ~ typeTree).map {
    case name ~ tt => ParamDef(name, tt)
  }

  // lazy val mbTypedParameters: Syntax[List[ParamDef]] = repsep(mbTypedParameter, ",").map(_.toList)

  // lazy val mbTypedParameter: Syntax[ParamDef] = (identifier ~ opt(":" ~ typeTree)).map {
  //   case name ~ None => ParamDef(name, None)
  //   case name ~ Some(_ ~ tt) => ParamDef(name, Some(tt))
  // }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] =
    recursive {
      def getArgList(preparsed: TypeTree): List[TypeTree] = {
        preparsed.tpe match {
          case UnitType => List() // No args.
          case TupleType(xs) => xs // The tuple is already the arg list!
          case _ => List(preparsed)
        }
      }
      // A type, possibly a function type.
      rep1sep(bracketedType | unbracketedType, "=>").map {
        case ts =>
          // Function types are right-associative.
          val tsl: List[TypeTree] = ts.toList
          val last: TypeTree = ts.last.tpe match {
            case TupleType(t::Nil) => t
            case _ => ts.last
          }
          tsl.init.foldRight(last)(
            (x, acc) => 
              TypeTree(FunctionType(getArgList(x), acc)).setPos(x)
          ).setPos(tsl.head)
      }
    }

  // Simple type.
  lazy val unbracketedType: Syntax[TypeTree] = primitiveType | identifierType

  // Unit, singleton, or tuple type.
  lazy val bracketedType: Syntax[TypeTree] =
    recursive {
      ("(" ~ repsep(typeTree, ",") ~<~ ")").map {
        case br ~ ts =>
          val tts = ts.toList
          val res =
            if tts.length == 0 then
              TypeTree(UnitType) // Special case. Treat () as sugar for UnitType.
            else if tts.length == 1 then
              // Singleton.
              // Parentheses are redundant IF inner type is a tuple AND outer type is a function type. See Note [Tuple Calls].
              tts.head.tpe match {
                case TupleType(xs) => // We'll keep the outer parentheses (wrap the inside as a tuple) for now.
                  // This is done to denote ONE argument of TUPLE type instead of MULTIPLE arguments of tuple field types.
                  // We'll have to check outside whether it's a function type or no.
                  TypeTree(TupleType(tts))
                case _ => tts.head // Inside is not a tuple? Then the parentheses are redundant.
              }
            else
              TypeTree(TupleType(tts)) // Tuple.
          res.setPos(br)
      }
    }

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // A user-defined type (such as `List`).
  val identifierType: Syntax[TypeTree] = (identifier ~ opt("." ~>~ identifier)).map {
    case name ~ None => TypeTree(ClassType(QualifiedName(None, name)))
    case module ~ Some(name) => TypeTree(ClassType(QualifiedName(Some(module), name)))
  }

  // An expression. We break it into several layers (expr1, expr2, expr3, ...) in order to
  // embed precedence into the parser.
  lazy val expr: Syntax[Expr] =
    recursive {
      // First level of expressions.
      valExpr | expr1OrSequence
    }

  lazy val valExpr: Syntax[Expr] =
    recursive {
      (kw("val") ~ parameter ~<~ "=" ~ expr1 ~<~ ";" ~ expr).map {
        case vl ~ df ~ value ~ body => Let(df, value, body).setPos(vl)
      }
    }

  lazy val lambda: Syntax[Lambda] =
    recursive {
      // val singleParam = parameter.map(List(_))
      val multiParam = ("(" ~>~ parameters ~<~ ")")
      ("\\" ~ (multiParam) ~ opt(":" ~>~ typeTree) ~<~ "->" ~ expr1).map {
        case sl ~ params ~ retType ~ expr => Lambda(params, retType, expr).setPos(sl)
      }
    }
  
  lazy val expr1OrSequence: Syntax[Expr] =
    recursive {
      // Left-factor higher precedence expressions.
      (expr1 ~ opt(";" ~>~ expr)).map {
        case e ~ None => e
        case e1 ~ Some(e2) => Sequence(e1, e2).setPos(e1)
      }
    }

  // Non-let expression.
  lazy val expr1: Syntax[Expr] =
    recursive {
      // Left-factor second level of expressions. What follows is either nothing (i.e. 
      // the expression is an expression) or a chain of match expressions.
      lazy val ifOrMatch: Syntax[Expr] = {
        ((ifExpr | expr2) ~ many(matchBody)).map {
          case e ~ Seq() => e
          // TODO: setPos for Match
          case e ~ matches => matches.foldLeft(e)(Match.apply).setPos(e) // Fold any chained match expressions.
                                                                        // Any other use of match is either a subexpression or an error.
        }
      }
      lambda.up[Expr] | ifOrMatch
    }

  lazy val ifExpr: Syntax[Expr] =
    recursive {
      (kw("if") ~<~ "(" ~ expr ~<~ ")" ~<~ "{" ~ expr ~<~ "}" ~<~ kw("else") ~<~ "{" ~ expr ~<~ "}").map {
        case iff ~ eCond ~ eThen ~ eElse => Ite(eCond, eThen, eElse).setPos(iff)
      }
    }

  lazy val matchBody: Syntax[List[MatchCase]] =
    recursive {
      (kw("match") ~<~ "{" ~>~ many1(matchCase) ~<~ "}").map (_.toList) // Position unimportant here.
    }

  // A match case combinator.
  lazy val matchCase: Syntax[MatchCase] = (kw("case") ~ pattern ~<~ "=>" ~ expr).map {
    case c ~ pat ~ expr => MatchCase(pat, expr).setPos(c)
  }
  
  // Covers binary expressions.
  lazy val expr2: Syntax[Expr] =
    recursive {
      // Operands are higher-precedence expressions (unary expressions, identifiers, literals, errors, etc.).
      // i.e. the next layer of expressions.
      operators(expr3) (
        // Highest precedence at the top.
        op("*") | op("/") | op("%")   is LeftAssociative, // All these odd bois are left associative.
        op("+") | op("-") | op("++")  is LeftAssociative,
        op("<") | op("<=")            is LeftAssociative,
        op("==")                      is LeftAssociative,
        op("&&")                      is LeftAssociative,
        op("||")                      is LeftAssociative
      ) {
        // Apply the operator.
        case (l, op, r) =>
          Map[String, (Expr, Expr) => Expr](
            "+" -> Plus.apply,
            "-" -> Minus.apply,
            "*" -> Times.apply,
            "/" -> Div.apply,
            "%" -> Mod.apply,
            "<" -> LessThan.apply,
            "<=" -> LessEquals.apply,
            "&&" -> And.apply,
            "||" -> Or.apply,
            "==" -> Equals.apply,
            "++" -> Concat.apply,
          )(op)(l, r).setPos(l)
      }
    }

  // Unary operators.
  lazy val expr3: Syntax[Expr] =
    recursive {
      (opt(unaryOp) ~ expr4).map {
        case None ~ expr4 => expr4
        case Some(unaryFunc: (Expr => Expr)) ~ expr4 => unaryFunc(expr4)
      }
    }

  // Unary operation. There are only two...
  val unaryOp: Syntax[Expr => Expr] = 
    opPos("-").map { case (_, p) => (e: Expr) => Neg(e).setPos(p) } 
    | opPos("!").map { case (_, p) => (e: Expr) => Not(e).setPos(p) } 

  // Everything else: error, literals, calls, identifiers.
  // These peeps are on the highest precedence.
  lazy val expr4: Syntax[Expr] =
    recursive {
      // Handle calls in both identifier and bracket expr cases.
      // If it's a single call on an identifier, we want to register it as an identifier call.
      error | identifierAndCalls | bracketedExprAndCalls | otherLiteral.up[Expr]
    }

  lazy val error: Syntax[Expr] =
    recursive {
      (kw("error") ~<~ "(" ~ expr ~<~ ")").map { case e ~ expr => Error(expr).setPos(e) }
    }

  lazy val call: Syntax[List[Expr]] = recursive {
    ("(" ~>~ repsep(expr, ",") ~<~ ")").map (_.toList)
  }

  // A combined combinator for parsing stuff beginning with an identifier (either a variable identifier
  // or a call, possibly qualified).
  lazy val identifierAndCalls: Syntax[Expr] = 
    recursive {
      // Left-factor identifier to make grammar LL(1).
      val qualIdentifier = (identifierPos ~ opt("." ~>~ identifier)).map {
        case (id, pos) ~ None => (QualifiedName(None, id), pos)
        case (module, pos) ~ Some(name) => (QualifiedName(Some(module), name), pos)
      }
      
      (qualIdentifier ~ many(call)).map {
        case (qname, pos) ~ calls =>
          calls.toList match {
            case Nil => Variable(qname).setPos(pos) // No calls.
            // case args::rest => rest.foldLeft(IdentifierCall(qname, args))(Call.apply).setPos(pos)
            // TODO: setPos for Call
            case args => args.foldLeft(Variable(qname).setPos(pos))(Call.apply).setPos(pos)
          }
      }
    }

  // Left-factor the opening paren "(" to parse an unit literal, a tuple, or a subexpression.
  lazy val bracketedExprAndCalls: Syntax[Expr] = {
    // Value calls shouldn't be allowed on (unit) literals...
    val unitLiteral: Syntax[UnitLiteral] = ")".map(_ => UnitLiteral())
    // ...so they'll only be handled with tuples/subexprs
    lazy val tupleOrSubExpr: Syntax[Expr] = recursive {
      (rep1sep(expr, ",") ~<~ ")" ~ many(call)).map {
        case exs ~ calls =>
          val baseExpr = exs.toList match {
            case Nil => throw new java.lang.Error("You shouldn't be here.")
            case one::Nil => one // Singleton: directly unpack.
            case many => Tuple(many) // Multiple expressions -> Tuple.
          }
          // TODO: setPos for valueCall
          calls.foldLeft(baseExpr)(Call.apply)
      }
    }
    ("(" ~ (unitLiteral.up[Expr] | tupleOrSubExpr)).map { case a ~ expr => expr.setPos(a) }
  }

  // A (non-unit) literal expression. Handles all other literals.
  val otherLiteral: Syntax[Literal[_]] = accept(LiteralKind) {
    case a@StringLitToken(s) => StringLiteral(s).setPos(a)
    case a@IntLitToken(i) => IntLiteral(i).setPos(a)
    case a@BoolLitToken(b) => BooleanLiteral(b).setPos(a)
  }

  // A pattern as part of a match case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | idOrCaseClassPattern | bracketedPattern
  }

  val literalPattern: Syntax[Pattern] = otherLiteral.map { case pat => LiteralPattern(pat).setPos(pat) }

  val wildPattern: Syntax[Pattern] = kw("_").map(_ => WildcardPattern())

  lazy val idOrCaseClassPattern: Syntax[Pattern] = recursive {
    // Left-factor the identifier. Similar to `identifierOrCall`.
    (identifierPos ~ opt(opt("." ~>~ identifier) ~<~ "(" ~ repsep(pattern, ",") ~<~ ")")).map {
      case (name, pos) ~ None => IdPattern(name).setPos(pos)
      case (module, pos) ~ Some(Some(name) ~ pats) => CaseClassPattern(QualifiedName(Some(module), name), pats.toList).setPos(pos)
      case (name, pos) ~ Some(None ~ pats) => CaseClassPattern(QualifiedName(None, name), pats.toList).setPos(pos)
    }
  }

  lazy val bracketedPattern: Syntax[Pattern] = recursive {
    val unit = ")".map(_ => LiteralPattern(UnitLiteral()))
    lazy val tuple: Syntax[Pattern] = recursive {
      (rep1sep(pattern, ",") ~<~ ")").map (ps => TuplePattern(ps.toList))
    }
    ("(" ~ (unit | tuple)).map { case br ~ pat => pat.setPos(br) }
  }


  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = true
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
