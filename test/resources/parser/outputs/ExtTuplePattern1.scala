object TuplePattern {

  val a: (Int, L.List) =
    (1, L.Cons(2, L.Cons(3, L.Nil())));
  a match {
    case (_, L.Cons(h, t)) =>
      Std.printString(Std.intToString((h + 1)))
  }
}