object CollatzConjecture {
  import scala.annotation.tailrec

  private val zero: BigInt = BigInt(0)
  private val one: BigInt = BigInt(1)

  def main(args: Array[String]): Unit = {
    val (start, end) = (args.headOption.map(_.toInt).getOrElse(1), args.lift(1).map(_.toInt).getOrElse(15))
    val numbers: List[Int] = List.range(start, end + 1)

    println(prettyPrint(numbers.map(n => collatzList(n))))
  }

  // FROM:
  // [[1],[2,1],[3,10,5,16,8,4,2,1],[4,2,1],[5,16,8,4,2,1]]
  //
  // TO:
  //  1   2   3   4   5
  //      1  10   2  16
  //          5   1   8
  //         16       4
  //          8       2
  //          4       1
  //          2
  //          1
  def prettyPrint(list: List[List[BigInt]]): String = {
    val listLen = list.map(_.length).maxOption.getOrElse(0)
    val digitLen = list.flatten.maxOption.map(_.toString.length).getOrElse(0)
    (for {
      i <- 0 to listLen
    } yield {
      (for {
        ls <- list
        num = ls.lift(i)
      } yield {
        s"${num.map(_.toString.reverse.padTo(digitLen, ' ').reverse).getOrElse(" " * digitLen)}"
      }).mkString(" ")
    }).mkString("\n")
  }

  /**
   * Function to find the number of steps required to reach 1 for input number `n` in The Collatz conjecture
   * that can be summarized as follows:
   * (*) If `n` is even `->` divide `n` by 2 to get `n / 2`
   * (*) If `n` is odd `->` multiply `n` by 3 and add 1 to get `3n + 1`
   * (*) Repeat the process indefinitely until `1` reached
   * @param n  input number
   * @return  the number of steps required to reach 1
   * */
  def steps(n: Int): Option[Int] = {
    Option.when(n > 0)(collatz(n))
  }

  @tailrec
  def collatz(n: BigInt, i: Int = 0): Int = (n, n % 2) match {
    case (`one`, _)  => i
    case (_, `zero`) => collatz(n / 2, i + 1)
    case (_, `one`)  => collatz(3 * n + 1, i + 1)
  }

  @tailrec
  def collatzList(n: BigInt, list: List[BigInt] = Nil): List[BigInt] = (n, n % 2) match {
    case (`one`, _)  => (n :: list).reverse
    case (_, `zero`) => collatzList(n / 2, n :: list)
    case (_, `one`)  => collatzList(3 * n + 1, n :: list)
  }

}
