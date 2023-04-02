/**
 * Solve the Einstein's Puzzle.
 *
 * 1. There are five houses.
 * 2. The Englishman lives in the red house.
 * 3. The Spaniard owns the dog.
 * 4. Coffee is drunk in the green house.
 * 5. The Ukrainian drinks tea.
 * 6. The green house is immediately to the right of the ivory house.
 * 7. The Old Gold smoker owns snails.
 * 8. Kools are smoked in the yellow house.
 * 9. Milk is drunk in the middle house.
 * 10. The Norwegian lives in the first house.
 * 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
 * 12. Kools are smoked in the house next to the house where the horse is kept.
 * 13. The Lucky Strike smoker drinks orange juice.
 * 14. The Japanese smokes Parliaments.
 * 15. The Norwegian lives next to the blue house.
 *
 * Each of the five houses is painted a different color,
 * and their inhabitants are of different national extractions,
 * own different pets, drink different beverages
 * and smoke different brands of cigarettes.
 *
 * Which of the residents drinks water? Who owns the zebra?
 * */
object EinsteinPuzzle {
  import Color._
  import Resident._
  import Animal._
  import Drink._
  import Smoke._

  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Yellow extends Color
    case object Blue extends Color
    case object Ivory extends Color

    val all: List[Color] = List(Red, Green, Yellow, Blue, Ivory)
    def restOf(houses: List[House]): List[Color] = all diff houses.flatMap(_.color)
  }

  sealed trait Resident
  object Resident {
    case object Englishman extends Resident
    case object Spaniard extends Resident
    case object Ukrainian extends Resident
    case object Norwegian extends Resident
    case object Japanese extends Resident

    val all: List[Resident] = List(Englishman, Spaniard, Ukrainian, Norwegian, Japanese)
    def restOf(houses: List[House]): List[Resident] = all diff houses.flatMap(_.resident)
  }

  sealed trait Animal
  object Animal {
    case object Dog extends Animal
    case object Snails extends Animal
    case object Fox extends Animal
    case object Horse extends Animal
    case object Zebra extends Animal

    val all: List[Animal] = List(Dog, Snails, Fox, Horse, Zebra)
    def restOf(houses: List[House]): List[Animal] = all diff houses.flatMap(_.animal)
  }

  sealed trait Drink
  object Drink {
    case object Coffee extends Drink
    case object Tea extends Drink
    case object Milk extends Drink
    case object OrangeJuice extends Drink
    case object Water extends Drink

    val all: List[Drink] = List(Coffee, Tea, Milk, OrangeJuice, Water)
    def restOf(houses: List[House]): List[Drink] = all diff houses.flatMap(_.drink)
  }

  sealed trait Smoke
  object Smoke {
    case object TheOldGold extends Smoke
    case object Kools extends Smoke
    case object Chesterfields extends Smoke
    case object LuckyStrike extends Smoke
    case object Parliaments extends Smoke

    val all: List[Smoke] = List(TheOldGold, Kools, Chesterfields, LuckyStrike, Parliaments)
    def restOf(houses: List[House]): List[Smoke] = all diff houses.flatMap(_.smoke)
  }

  final case class House(
                          num: Int,
                          color: Option[Color],
                          resident: Option[Resident],
                          animal: Option[Animal],
                          drink: Option[Drink],
                          smoke: Option[Smoke]
                        ) {
    def isDefined: Boolean =
      color.isDefined &&
        resident.isDefined &&
        animal.isDefined &&
        drink.isDefined &&
        smoke.isDefined

    override def toString: String =
      f"House(num=$num%1d," +
        f"color=[${color.map(_.toString).getOrElse("")}%8s]," +
        f"resident=[${resident.map(_.toString).getOrElse("")}%10s]," +
        f"animal=[${animal.map(_.toString).getOrElse("")}%10s]," +
        f"drink=[${drink.map(_.toString).getOrElse("")}%12s]," +
        f"smoke=[${smoke.map(_.toString).getOrElse("")}%15s])"
  }

  object House {
    def empty(i: Int): House = House(i, None, None, None, None, None)

    def print(houses: List[House]): Unit = {
      houses.sortBy(_.num).foreach(println)
      println()
    }
  }

  final case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  /** Find all houses in the Einstein's Puzzle
   * @return house resident who drinks water and house resident who owns zebra
   * */
  def solve: Option[Solution] = {
    ???
  }
}
