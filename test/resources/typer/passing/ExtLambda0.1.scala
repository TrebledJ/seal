object Lambda {
  val f: Int => Int = \(x: Int) -> x + 1;
  Std.printInt(f(1))
}
