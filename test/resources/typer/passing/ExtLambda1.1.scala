object Lambda {
  val f: Int => String = (\(x: Int) -> Std.intToString(x + 1));
  Std.printString(f(1))
}
