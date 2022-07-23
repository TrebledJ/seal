object Ext {
  def max(a: Int, b: Int): Int = {
    if (gt(a, b)) { a } else { b }
  }

  def min(a: Int, b: Int): Int = {
    if (a < b) { a } else { b }
  }

  def gt(a: Int, b: Int): Boolean = { !(a <= b) }
  def geq(a: Int, b: Int): Boolean = { !(a < b) }
}