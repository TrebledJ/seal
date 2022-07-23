object LambdaError {
  val f: Int => String = \(x: Int): Int -> x + 1;
  Std.printInt(f(1))
}
