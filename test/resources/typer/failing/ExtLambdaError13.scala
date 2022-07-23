object LambdaError {
  val x: Int = (\() -> (1, ("a", 2, false), true))()(1)(2);
  Std.printInt(x)
}
