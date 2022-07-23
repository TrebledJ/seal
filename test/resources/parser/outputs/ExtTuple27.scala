object Tuple {

  def foo(x: Unit, y: Unit): Unit = {
    ()
  }

  val f: (Unit, Unit) => Unit =
    foo;
  f((), ())
}