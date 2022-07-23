object MatchError {
  "123" match {
      case 0 => "yay"
      case 1 => "wow"
      case _ => "woohoo"
  }
}
