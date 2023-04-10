import scala.util.Try
import scala.util.parsing.combinator._

object Calculator extends RegexParsers {
  /**
   * Function to evaluate arithmetics operations
   * @param eq  the equation to evaluate
   * @return  some result number if equation is correct
   */
  def evaluate(eq: String): Option[Int] = Try(parseAll(expr, eq)) match {
    case scala.util.Success(Success(res, _)) => Some(res.toInt)
    case _ => None
  }

  // 3[2434](.[2342])
  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }

  // 3[2434](.[2342]) ЛИБО ( выражение === expr )
  def factor: Parser[Double] = number | "(" ~> expr <~ ")"

  // X*Y*Z/C -> ((X*Y)*Z)/C
  def term  : Parser[Double] = factor ~
    rep( "*" ~ log(factor)("Mult fact") | "/" ~ log(factor)("Div fact")) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  // T1+T2-T3, Ti=X*Y*Z/C
  def expr  : Parser[Double] = term ~
    rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    case number ~ list => (number /: list) {
      // same as before, using alternate name for /:
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}
