object MatchClass {
  val x: L.List = L.Cons(1, L.Cons(2, L.Nil()));
  x match {
      case L.Nil() => "empty"
      case L.Cons(0, L.Nil()) => "singleton, head == 0"
      case L.Cons(_, L.Nil()) => "singleton"
      case _ => "other"
  }
}
