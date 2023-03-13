object Alphametics {
  /**
   * Function to solve alphametics puzzles
   * Alphametics is a puzzle where letters in words are replaced with digits.
   *  Every letter in words should be replaced with digit 0 to 9
   *  Every letter in words should be replaced by a different digit
   *  The leading digit of number must not be zero
   *  Words translated into numbers should make a valid sum
   * @param eq  the equation among unknown numbers, whose digits are represented by letters of the alphabet
   * @return  some map with identification of the value of each letter if solution exists otherwise none
   */
  def solve(eq: String): Option[Map[Char, Int]] = ???
}
