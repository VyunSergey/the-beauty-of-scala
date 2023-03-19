import scala.annotation.tailrec

object CollatzConjecture {

  def steps(n: Int): Option[Int] = {
    @tailrec
    def collatz(n: BigInt, res: Int = 0): Int = {
      println((n, res))
      if (n == 1) res
      else if (n % 2 == 0) collatz(n / 2, res + 1)
      else collatz(3 * n + 1, res + 1)
    }
    if (n > 0) Some(collatz(n)) else None
  }
}
