import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AlphameticsTest extends AnyFunSuite with Matchers {

  test("puzzle with three letters") {
    println(1)
    Alphametics.solve("I + BB == ILL") should be(
      Some(Map('B' -> 9, 'I' -> 1, 'L' -> 0)))
  }

  test("solution must have unique value for each letter") {
    println(2)
    Alphametics.solve("A == B") should be(None)
  }

  test("leading zero solution is invalid") {
    println(3)
    Alphametics.solve("ACA + DD == BD") should be(None)
  }

  test("puzzle with two digits final carry") {
    println(4)
    Alphametics.solve("A + A + A + A + A + A + A + A + A + A + A + B == BCC") should be(
      Some(Map('A' -> 9, 'B' -> 1, 'C' -> 0)))
  }

  test("puzzle with four letters") {
    println(5)
    Alphametics.solve("AS + A == MOM") should be(
      Some(Map('A' -> 9, 'M' -> 1, 'O' -> 0, 'S' -> 2)))
  }

  test("puzzle with six letters") {
    println(6)
    Alphametics.solve("NO + NO + TOO == LATE") should be(
      Some(Map('A' -> 0, 'E' -> 2, 'L' -> 1, 'N' -> 7, 'O' -> 4, 'T' -> 9)))
  }

  test("puzzle with seven letters") {
    println(7)
    Alphametics.solve("HE + SEES + THE == LIGHT") should be(
      Some(
        Map('E' -> 4,
            'G' -> 2,
            'H' -> 5,
            'I' -> 0,
            'L' -> 1,
            'S' -> 9,
            'T' -> 7)))
  }

  test("puzzle with eight letters") {
    println(8)
    Alphametics.solve("SEND + MORE == MONEY") should be(
      Some(
        Map('D' -> 7,
            'E' -> 5,
            'M' -> 1,
            'N' -> 6,
            'O' -> 0,
            'R' -> 8,
            'S' -> 9,
            'Y' -> 2)))
  }

  test("puzzle with ten letters") {
    println(9)
    Alphametics.solve("AND + A + STRONG + OFFENSE + AS + A + GOOD == DEFENSE") should be(
      Some(
        Map('A' -> 5,
            'D' -> 3,
            'E' -> 4,
            'F' -> 7,
            'G' -> 8,
            'N' -> 0,
            'O' -> 2,
            'R' -> 1,
            'S' -> 6,
            'T' -> 9)))
  }

  test("puzzle with ten letters and a lot of addends") {
    println(10)
    Alphametics.solve("SO + MANY + MORE + MEN + SEEM + TO + SAY + THAT + " +
      "THEY + MAY + SOON + TRY + TO + STAY + AT + HOME + " +
      "SO + AS + TO + SEE + OR + HEAR + THE + SAME + ONE + " +
      "MAN + TRY + TO + MEET + THE + TEAM + ON + THE + " +
      "MOON + AS + HE + HAS + AT + THE + OTHER + TEN == TESTS") should be(
      Some(
        Map('E' -> 0,
            'N' -> 5,
            'T' -> 8,
            'Y' -> 4,
            'A' -> 6,
            'M' -> 3,
            'H' -> 1,
            'R' -> 2,
            'O' -> 9,
            'S' -> 7)))
  }

  test("puzzle with ten letters and 199 addends") {
    println(11)
    Alphametics.solve(
      "THIS + A + FIRE + THEREFORE + FOR + ALL + HISTORIES + I + TELL + A + TALE + " +
        "THAT + FALSIFIES + ITS + TITLE + TIS + A + LIE + THE + TALE + " +
        "OF + THE + LAST + FIRE + HORSES + LATE + AFTER + THE + FIRST + " +
        "FATHERS + FORESEE + THE + HORRORS + THE + LAST + FREE + TROLL + " +
        "TERRIFIES + THE + HORSES + OF + FIRE + THE + TROLL + " +
        "RESTS + AT + THE + HOLE + OF + LOSSES + IT + IS + THERE + " +
        "THAT + SHE + STORES + ROLES + OF + LEATHERS + AFTER + SHE + " +
        "SATISFIES + HER + HATE + OFF + THOSE + FEARS + A + TASTE + RISES + " +
        "AS + SHE + HEARS + THE + LEAST + FAR + HORSE + THOSE + FAST + " +
        "HORSES + THAT + FIRST + HEAR + THE + TROLL + FLEE + OFF + TO + " +
        "THE + FOREST + THE + HORSES + THAT + ALERTS + RAISE + THE + STARES + " +
        "OF + THE + OTHERS + AS + THE + TROLL + ASSAILS + AT + THE + TOTAL + " +
        "SHIFT + HER + TEETH + TEAR + HOOF + OFF + TORSO + AS + THE + LAST + " +
        "HORSE + FORFEITS + ITS + LIFE + THE + FIRST + FATHERS + HEAR + " +
        "OF + THE + HORRORS + THEIR + FEARS + THAT + THE + FIRES + " +
        "FOR + THEIR + FEASTS + ARREST + AS + THE + FIRST + FATHERS + " +
        "RESETTLE + THE + LAST + OF + THE + FIRE + HORSES + THE + " +
        "LAST + TROLL + HARASSES + THE + FOREST + HEART + FREE + " +
        "AT + LAST + OF + THE + LAST + TROLL + ALL + OFFER + THEIR + " +
        "FIRE + HEAT + TO + THE + ASSISTERS + FAR + OFF + THE + " +
        "TROLL + FASTS + ITS + LIFE + SHORTER + AS + STARS + " +
        "RISE + THE + HORSES + REST + SAFE + AFTER + ALL + SHARE + " +
        "HOT + FISH + AS + THEIR + AFFILIATES + TAILOR + A + " +
        "ROOFS + FOR + THEIR + SAFE == FORTRESSES") should be(
      Some(
        Map('A' -> 1,
            'E' -> 0,
            'F' -> 5,
            'H' -> 8,
            'I' -> 7,
            'L' -> 2,
            'O' -> 6,
            'R' -> 3,
            'S' -> 4,
            'T' -> 9)))
  }
}
