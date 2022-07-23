object Tuple {
  def foo(x: (), y: (())): () = { () }
  val f: ((), (())) => (()) = foo;
  // val g: ((Int, String)) => Boolean = foo; // OK in Scala, error in Seal.
  f((), ())
}