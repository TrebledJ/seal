object Lambda {
  val f: Int => Int => Int = \(x: Int): Int => Int -> \(y: Int) -> x + y;
  Std.printInt(f(1)(2))
}
