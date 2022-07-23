object Call {

  def f(s: String): Unit = {
    Std.printString(("teehee-- " ++ s))
  }

  (if(true) {
    Std.printString
  } else {
    f
  })("hello world");
  (if(false) {
    Std.printString
  } else {
    f
  })("hello world")
}