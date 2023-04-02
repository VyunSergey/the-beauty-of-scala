import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import EinsteinPuzzle.{Solution, Resident}

class EinsteinPuzzleTest extends AnyFunSuite with Matchers {
  test("solve the Zebra Puzzle") {
    EinsteinPuzzle.solve should be (Some(Solution(waterDrinker = Resident.Norwegian, zebraOwner = Resident.Japanese)))
  }
}
