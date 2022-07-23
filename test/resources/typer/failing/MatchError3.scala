object MatchError3 {
  val what: Unit = ();
  0 match {
      case 0 => "yay"
      case 1 => what
      case _ => "woohoo"
  }
}
