object Lambda {

  val f: Int => Int =
    \(x: Int): Int -> (if(((x % 2) == 0)) {
      1
    } else {
      0
    });
  Std.printInt(f(42));
  Std.printInt(f(41));
  Std.printInt(f(40))
}