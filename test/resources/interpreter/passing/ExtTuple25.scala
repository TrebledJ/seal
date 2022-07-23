object Tuple {
  def foo(x: (((Int))), y: (String)): Boolean = { Std.printInt(x); Std.printString(y); true }
  val f: ((((((((((Int)))), ((String))) => Boolean))))) = foo;
  // val g: ((Int, String)) => Boolean = foo; // OK in Scala, error in Seal.
  f(1, "abc")
}