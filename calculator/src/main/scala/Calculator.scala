import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object Calculator extends RegexParsers {
  /**
   * Function to evaluate arithmetics operations
   * @param eq  the equation to evaluate
   * @return  some result number if equation is correct
   */
  def evaluate(eq: String): Option[Int] = Try(parseAll(expr, eq)) match {
    case scala.util.Success(Success(result, _)) => Some(result.toInt)
    case _ => None
  }

  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }

  def factor: Parser[Double] = number | "(" ~> expr <~ ")"

  def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    case number ~ list => (number /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}
