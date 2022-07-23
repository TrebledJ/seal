object LambdaError {
  val f: Int => Int = \(x: String): Int -> "x + 1";
  Std.printInt(f(1))
}
