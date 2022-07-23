object LambdaError {
  val x: String = "";
  val f: Int => Int = \(x: Int): Int -> (val x: Boolean = true; x + 1);
  Std.printInt(f(1))
}
