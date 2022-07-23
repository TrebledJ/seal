object Lambda {
  val x: String = "";
  val f: Int => Int = \(x: Int): Int -> x + 1;
  Std.printInt(f(1))
}
