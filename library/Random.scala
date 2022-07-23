object Random {
  // Generates a random number between [0, n).
  def int(n: Int): Int = {
    error("") // Stub implementation
  }

  // Generates a random number between [min, max].
  def range(min: Int, max: Int): Int = {
    int(max - min + 1) + min
  }

  def oneIn(n: Int): Boolean = {
    int(n) == 0
  }
}