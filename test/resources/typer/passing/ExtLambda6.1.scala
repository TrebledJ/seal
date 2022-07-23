object Lambda {
  val f: Int => Int = \(x: Int) -> (\(y: Int) -> x + y)(1);
  Std.printInt(f(41))
}
