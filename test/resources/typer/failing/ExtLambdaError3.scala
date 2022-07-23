object LambdaError {
  val x: String = "";
  val f: Int => Int = \(x: Int): Int -> (Std.printString(x); 0);
  Std.printInt(f(1))
}
