object Alphametics {
  import scala.annotation.tailrec

  private val alphabet: Set[Char] = Set('A' to 'Z': _*)
  private val digits: Set[Int] = Set(0 to 9: _*)

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
  def solve(eq: String): Option[Map[Char, Int]] =
    eq.split(" == ").toList match {
      case left :: right :: Nil if valid(left) && valid(right) =>
        solveEq(left.split(" \\+ "), right)
      case _ => None
    }

  // prepare method for creating list of equations to solve
  def solveEq(leftTokens: Array[String], right: String): Option[Map[Char, Int]] = {
    val leftEqs: List[List[(Char, Int)]] = equations(leftTokens)
    val rightEqs: List[Option[Char]] = right.toList.reverse.map(Some(_))
    val unionEqs: List[(List[(Char, Int)], Option[Char])] = leftEqs.zipAll(rightEqs, Nil, None)
    val lts = letters(leftTokens) ++ letters(right.split(" \\+ "))
    val sts = startLetters(leftTokens) ++ startLetters(right.split(" \\+ "))
    val conditions = lts.map(c => c -> digits).toMap ++ sts.map(c => c -> (digits - 0)).toMap
    solveEqs(unionEqs, 0, conditions, Map.empty[Char, Int])
  }

  // recursive method for solving list of equations from last digit to first
  def solveEqs(equations: List[(List[(Char, Int)], Option[Char])],
               accum: Int,
               conditions: Map[Char, Set[Int]],
               known: Map[Char, Int]): Option[Map[Char, Int]] =
    equations match {
      case Nil =>
        // known is solution
        Some(known)
      case ((c, n) :: cs, Some(cr)) :: eqs if known.contains(c) =>
        // replace known char with digit
        solveEqs((cs, Some(cr)) :: eqs, accum + n * known(c), conditions, known)
      case ((c, n) :: cs, Some(cr)) :: eqs if !known.contains(c) =>
        // iterate over all available digits for unknown char
        (conditions(c) diff known.values.toSet)
          .foldLeft(Option.empty[Map[Char, Int]]) { case (ans, d) =>
            ans orElse solveEqs((cs, Some(cr)) :: eqs, accum + n * d, conditions, known ++ Map(c -> d))
          }
      case (Nil, Some(cr)) :: eqs if known.contains(cr) =>
        // check accum equals known digit for right side char
        Some(cr).filter(accum % 10 == known(_)).flatMap { _ =>
          solveEqs(eqs, accum / 10, conditions, known)
        }
      case (Nil, Some(cr)) :: eqs if !known.contains(cr) &&
        (conditions(cr) diff known.values.toSet).contains(accum % 10) =>
        // find digit value for right side char
        solveEqs(eqs, accum / 10, conditions, known ++ Map(cr -> accum % 10))
      case _ => None
    }

  // TEXT -> (A to Z)[_+_(A to Z)]*...
  def valid(part: String): Boolean = {
    part.split(" \\+ ").forall(_.forall(c => alphabet.contains(c)))
  }

  // A + BB + AA + CDE -> [[(A,2), (B,1), (E,1)], [(B,1), (A,1), (D,1)], [(C,1)]]
  def equations(tokens: Array[String]): List[List[(Char, Int)]] = {
    tokens.map(_.toList.map(c => List(c)).reverse)
      .reduce(union(_, _))
      .map(_.groupMapReduce(x => x)(_ => 1)(_ + _).toList)
  }

  // [[A, B], [C]] UNION [[C], [D, E], [F]] -> [[A, B, C], [C, D, E], [F]]
  @tailrec
  def union[A](left: List[List[A]], right: List[List[A]], res: List[List[A]] = Nil): List[List[A]] =
    (left, right) match {
      case (l :: ls, r :: rs) => union(ls, rs, (l ++ r) :: res)
      case (l :: ls, Nil)     => union(ls, Nil, l :: res)
      case (Nil, r :: rs)     => union(Nil, rs, r :: res)
      case (Nil, Nil)         => res.reverse
    }

  // A + BB + CDE -> [A, B, C, D, E]
  def letters(tokens: Array[String]): Set[Char] = {
    tokens.flatMap(_.toCharArray).toSet
  }

  // A + BB + CDE -> [A, B, C]
  def startLetters(tokens: Array[String]): Set[Char] = {
    tokens.flatMap(_.headOption).toSet
  }

}
