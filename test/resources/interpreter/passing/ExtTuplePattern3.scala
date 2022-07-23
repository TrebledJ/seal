object TuplePattern {
  val b: Int = ((1, ((), L.Nil())), true) match {
    case ((2, _), _) => 0
    case ((1, ((), L.Cons(1, L.Nil()))), false) => 1
    case ((x, ((), L.Nil())), true) => x*100
    case _ => 3
  };
  Std.printString(Std.intToString(b))
}