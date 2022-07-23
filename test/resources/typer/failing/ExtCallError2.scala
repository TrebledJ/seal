object CallError {
  // Different function types!
  (true match {
    case true => Std.printString
    case false => Std.intToString
  })("hello there--")
}
