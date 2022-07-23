object Lambda {
  val x: String = "a";
  val y: Int = 3;
  val f: Int => Int = (\(x: Int) -> (val x: Int = 2; x + 1 + y));
  Std.printInt(f(1))
}
