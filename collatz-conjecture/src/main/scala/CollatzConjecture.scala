object CollatzConjecture {

  /**
   * Function to find the number of steps required to reach 1 for input number `n` in The Collatz conjecture
   * that can be summarized as follows:
   * (*) If `n` is even `->` divide `n` by 2 to get `n / 2`
   * (*) If `n` is odd `->` multiply `n` by 3 and add 1 to get `3n + 1`
   * (*) Repeat the process indefinitely until `1` reached
   * @param n  input number
   * @return  the number of steps required to reach 1
   * */
  def steps(n: Int): Option[Int] = ???
}
