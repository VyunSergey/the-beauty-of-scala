import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CalculatorTest extends AnyFunSuite with Matchers {

  test("test 0") {
    Calculator.evaluate("1") should be(Some(1))
  }

  test("test 1") {
    Calculator.evaluate("1 + 3") should be(Some(4))
  }

  test("test 2") {
    Calculator.evaluate("") should be(None)
  }

  test("test 3") {
    Calculator.evaluate("1-3*2") should be(Some(-5))
  }

  test("test 4") {
    Calculator.evaluate("(1-3)*2") should be(Some(-4))
  }

  test("test 5") {
    Calculator.evaluate("(1 - 7)*1 * 3-(12 + (7 * 3)) + 5") should be(Some(-46))
  }

  test("test 6") {
    Calculator.evaluate("(1 - 7)*1 - 3*(12 + ((7 * 3)) + 5)") should be(Some(-120))
  }

  test("test 7") {
    Calculator.evaluate("6-1*(5-2*(4-3*(3-4*(2-1))))") should be(Some(15))
  }

  test("test 8") {
    Calculator.evaluate("5-((6+2)+4-(5+6-5+5))*6-7-(4-3+2-3)*4+5") should be(Some(-3))
  }

  test("test 9") {
    Calculator.evaluate("2+3-8*2+3-3*4-6+7*4-4+1+2-3+2*4-3*5-6+9*8-9+7-8-6+6*0-6+7-5") should be(Some(37))
  }

  test("test 10") {
    Calculator.evaluate("2+3-(8*(2+3-3)*4-(6+7)*4)-(4+1+2-3+2)*4+((0-3*5-6+9)*8-(9+7))-8-6+(6*(0-6+7-5))") should be(Some(-181))
  }
}
