object Alphametics {
  val alphabet: Set[Char] = ('A' to 'Z').toSet
  val digits: Set[Int] = (0 to 9).toSet
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
  def solve(eq: String): Option[Map[Char, Int]] = eq.split(" == ").toList match {
    case left :: right :: Nil => solveEq(left.split(" \\+ "), right)
    case _ => None
  }

  def solveEq(left: Array[String], right: String): Option[Map[Char, Int]] = {
    // [I, BB]  ILL
    // (I + B) % 10 = L               F0 % 10              = L,  F0 = I + B
    // ((I + B) / 10 + B) % 10 = L    (F0 / 10) + F1) % 10 = L,  F1 = B
    // ((I + B) / 10 + B) / 10 = I    (F1 / 10) + F2) % 10 = I,  F2 = _

    // [F0, F1, F2]
    val eqs: List[List[(Char, Int)]] = equations(left).map(_.groupMapReduce(x => x)(_ => 1)(_ + _).toList)
    // [L, L, I]
    val target: List[Char] = right.toList.reverse
    // {I, B, L}
    val chars: Set[Char] = (left.flatMap(_.toList) ++ right.toList).toSet
    // {I, B}
    val capChars: Set[Char] = (left.flatMap(_.headOption) ++ Array(right).flatMap(_.headOption)).toSet
    // (I -> 1-9, B -> 1-9, L -> 0-9)
    val cond: Map[Char, List[Int]] =
      chars.map(c => c -> digits.toList).toMap ++
        capChars.map(c => c -> (digits - 0).toList)

    solveEqs(eqs zipAll (target, Nil, '@'), cond, 0, Map.empty[Char, Int])
  }

  def solveEqs(eqs: List[(List[(Char, Int)], Char)],
               cond: Map[Char, List[Int]],
               accum: Int,
               known: Map[Char, Int]): Option[Map[Char, Int]] = eqs match {
    case Nil if accum == 0 => Some(known)
    case (Nil, rc) :: tail if known.contains(rc) =>
      for {
        _   <- Option.when(known(rc) == accum % 10)(())
        res <- solveEqs(tail, cond, accum / 10, known)
      } yield res
    case (Nil, rc) :: tail if !known.contains(rc) =>
      for {
        _   <- Option.when(!known.exists(_._2 == accum % 10))(())
        res <- solveEqs(tail, cond, accum / 10, known ++ Map(rc -> accum % 10))
      } yield res
    case ((c, n) :: rest, rc) :: tail if known.contains(c) =>
      solveEqs((rest, rc) :: tail, cond, accum + known(c) * n, known)
    case ((c, n) :: rest, rc) :: tail if !known.contains(c) =>
      Console.out.println(s"char=$c num=$n eq=${(c, n) :: rest} target=$rc")
      Console.flush()

      cond(c).foldLeft(Option.empty[Map[Char, Int]]) { case (res, d) =>
        res orElse solveEqs((rest, rc) :: tail, cond, accum + d * n, known ++ Map(c -> d))
      }
    case _ => None
  }

  // [I, BB] -> [[I, B], [B], []]  =  [F0, F1, F2]
  def equations(left: Array[String]): List[List[Char]] = {
    // [I, BB] -> len(BB) = 2
    val maxCharsLen = left.map(_.length).maxOption.getOrElse(0)
    for {
      i <- (0 to maxCharsLen).toList
    } yield {
      (for {
        j <- left.indices.toList
      } yield {
        left(j).reverse.lift(i)
      }) collect { case Some(c) => c }
    }
  }

}
