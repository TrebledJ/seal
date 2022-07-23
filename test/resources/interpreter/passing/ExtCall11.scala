object Call {
  def f(s: String): Unit = { Std.printString(s) }
  (true match {
    case true => Std.printString
    case false => f
  })("hello there--")
}
