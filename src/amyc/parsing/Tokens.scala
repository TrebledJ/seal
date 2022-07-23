package amyc
package parsing

import utils.Positioned


enum Token extends Positioned with Product:
  case KeywordToken(value: String)    // e.g. keyword "if"
  case IdentifierToken(name: String)  // e.g. variable name "x" 
  case PrimTypeToken(value: String)   // e.g. primitive type "Int"
  case IntLitToken(value: Int)        // e.g. integer literal "123"
  case StringLitToken(value: String)
  case BoolLitToken(value: Boolean)
  case DelimiterToken(value: String)  // .,:;(){}[]= and =>
  case OperatorToken(name: String)    // e.g. "+"
  case CommentToken(text: String)     // e.g. "// this is a comment"
  case SpaceToken()                   // e.g. "\n  "
  case ErrorToken(content: String)
  case EOFToken()                     // special token at the end of file
  
  override def toString =
    productPrefix + productIterator.mkString("(", ",", ")") + "(" + position.withoutFile + ")"


enum TokenKind:
  case KeywordKind(value: String)
  case IdentifierKind
  case PrimTypeKind
  case LiteralKind
  case DelimiterKind(value: String)
  case OperatorKind(value: String)
  case EOFKind
  case NoKind

  override def toString: String = this match
    case KeywordKind(value) => value
    case IdentifierKind => "<Identifier>"
    case PrimTypeKind => "<Primitive Type>"
    case LiteralKind => "<Literal>"
    case DelimiterKind(value: String) => value
    case OperatorKind(value: String) => value
    case EOFKind => "<EOF>"
    case NoKind => "<???>"  

object TokenKind:
  import Token._

  def of(token: Token): TokenKind = token match
    case KeywordToken(value) => KeywordKind(value)
    case IdentifierToken(_) => IdentifierKind
    case PrimTypeToken(_) => PrimTypeKind
    case BoolLitToken(_) => LiteralKind
    case IntLitToken(_) => LiteralKind
    case StringLitToken(_) => LiteralKind
    case DelimiterToken(value) => DelimiterKind(value)
    case OperatorToken(value) => OperatorKind(value)
    case EOFToken() => EOFKind
    case _ => NoKind

