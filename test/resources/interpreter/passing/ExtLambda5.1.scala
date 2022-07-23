object Lambda {
  val f: Int => Int => Int = \(x: Int) -> \(y: Int) -> x + y;
  Std.printInt(f(1)(2))
}
