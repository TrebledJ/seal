object Type4 {
  def f(a: String => ()): () = {
    a("look at this!")
  }
  def g(a: (String) => ((String) => (String))): String = {
    val b: String => String = a("hello");
    b(" world")
  }
  f(Std.printString)
}
