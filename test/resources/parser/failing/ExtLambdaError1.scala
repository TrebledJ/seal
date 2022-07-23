object LambdaError {
  val f: Int => Int = (\(x: Int): Int x + 1);
  Std.printInt(f(1))
}
