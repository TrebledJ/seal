object Match {
  val x: Int = 1 + 2;
  (x * 2) match {
      case 0 => "yay"
      case 1 => "wow"
      case _ => "woohoo"
  }
}
