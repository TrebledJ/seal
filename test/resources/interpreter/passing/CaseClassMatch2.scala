object CaseClassMatch2 {
  def f(l: L.List): String = {
    l match {
      case L.Nil() => "0"
      case L.Cons(x, L.Cons(y, L.Nil())) => "2 - " ++ Std.intToString(x) ++ ", " ++ Std.intToString(y)
      case L.Cons(_, t) => "1"
    }
  }

  val x0: L.List = L.Nil();
  val x1: L.List = L.Cons(1, L.Nil());
  val x2: L.List = L.Cons(1, L.Cons(2, L.Nil()));

  Std.printString(f(x0));
  Std.printString(f(x1));
  Std.printString(f(x2))
}