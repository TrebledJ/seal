object Lambda_0 {

  def foo_0(): Unit => Int = {
    val x_0: Int =
      1;
    val f_0: Unit => Int =
      \() -> x_0;
    f_0
  }

  val x_1: String =
    "";
  foo_0()()
}