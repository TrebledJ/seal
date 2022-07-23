object Lambda {
  val f: Int = \(x: Int) -> (\(y: Int) -> x + y);
  Std.printInt(f(41))
}
