object Lambda {
  val f: Int => Int => Int = \(x: Int) -> \(y: Int): Int -> x + y;
  Std.printInt(f(1)(2))
}
