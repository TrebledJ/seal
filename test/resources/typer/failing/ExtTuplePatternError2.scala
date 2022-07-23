object TuplePattern {
  ((1, ("a", L.Nil())), true) match {
    case ((2, _), 0) => 0
    case ((_, ("abc", _)), _) => 1
    case ((1, ("a", L.Cons(1, L.Nil()))), false) => 2
    case _ => 3
  }
}