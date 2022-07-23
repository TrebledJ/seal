object LambdaError {
  val f: Int => Int = (\(x: Int): Int ->);
  Std.printInt(f(1))
}
