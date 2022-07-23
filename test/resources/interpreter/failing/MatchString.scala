object StringMatch {
  "a" match {
    case "a" => true
    case _ => error("strings should never match")
  }
}