object Tuple {
  def f(a: (Int, String, Boolean, String)): String = {
    val b: Int = if (a(2)) {
      a(0) * a(0)
    } else {
      a(0) + 1
    };
    b match {
      case 0 => a(1) ++ a(3)
      case _ => a(3) ++ a(1)
    }
  }

  Std.printString(f((0, "Hello ", true, "world!")));
  Std.printString(f((1, "meet you!", true, "Nice to ")));
  Std.printString(f((0, "you do?", false, "How do ")))
}