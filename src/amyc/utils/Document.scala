package amyc.utils

// A structured document to be printed with nice indentation
enum Document:

  case Indented(content: Document)
  case Unindented(content: Document)
  case Stacked(docs: List[Document], emptyLines: Boolean = false)
  case Lined(docs: List[Document], separator: Document)
  case Raw(s: String)

  def <:>(other: Document) = line(List(this, other))

  def print: String = {
    val sb = new StringBuffer()

    def rec(d: Document)(implicit ind: Int, first: Boolean): Unit = d match {
      case Raw(s) =>
        if (first && s.nonEmpty) sb append ("  " * ind)
        sb append s
      case Indented(doc) =>
        rec(doc)(ind + 1, first)
      case Unindented(doc) =>
        assume(ind > 0)
        rec(doc)(ind - 1, first)
      case Lined(Nil, _) => // skip
      case Lined(docs, sep) =>
        rec(docs.head)
        docs.tail foreach { doc =>
          rec(sep)(ind, false)
          rec(doc)(ind, false)
        }
      case Stacked(Nil, _) => // skip
      case Stacked(docs, emptyLines) =>
        rec(docs.head)
        docs.tail foreach { doc =>
          sb append "\n"
          if (emptyLines) sb append "\n"
          rec(doc)(ind, true)
        }
    }

    rec(this)(0, true)
    sb.toString
  }
export Document.*

def stack(docs: Document*): Stacked = Stacked(docs.toList)
def line(docs: List[Document]) = Lined(docs, separator = Raw(""))
