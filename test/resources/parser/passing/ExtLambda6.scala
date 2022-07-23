object Lambda {
  val f: Int => Int = \(x: Int): Int -> (\(y: Int): Int -> x + y)(1);
  Std.printInt(f(41))
}
