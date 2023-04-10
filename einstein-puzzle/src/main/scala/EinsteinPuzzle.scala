import scala.collection.mutable

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
    def fill(houses: List[House], cond: List[House] => Boolean): Iterator[List[House]] = {
      val colors: List[Color] = restOf(houses)
      for {
        comb <- colors.permutations
        housesUpd = (houses.filter(_.color.isEmpty) zip comb).map { case (house, color) =>
          house.copy(color = Some(color))
        } ++ houses.filter(_.color.nonEmpty) if cond(housesUpd)
      } yield housesUpd
    }
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
    def fill(houses: List[House], cond: List[House] => Boolean): Iterator[List[House]] = {
      val residents: List[Resident] = restOf(houses)
      for {
        comb <- residents.permutations
        housesUpd = (houses.filter(_.resident.isEmpty) zip comb).map { case (house, resident) =>
          house.copy(resident = Some(resident))
        } ++ houses.filter(_.resident.nonEmpty) if cond(housesUpd)
      } yield housesUpd
    }
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
    def fill(houses: List[House], cond: List[House] => Boolean): Iterator[List[House]] = {
      val animals: List[Animal] = restOf(houses)
      for {
        comb <- animals.permutations
        housesUpd = (houses.filter(_.animal.isEmpty) zip comb).map { case (house, animal) =>
          house.copy(animal = Some(animal))
        } ++ houses.filter(_.animal.nonEmpty) if cond(housesUpd)
      } yield housesUpd
    }
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
    def fill(houses: List[House], cond: List[House] => Boolean): Iterator[List[House]] = {
      val drinks: List[Drink] = restOf(houses)
      for {
        comb <- drinks.permutations
        housesUpd = (houses.filter(_.drink.isEmpty) zip comb).map { case (house, drink) =>
          house.copy(drink = Some(drink))
        } ++ houses.filter(_.drink.nonEmpty) if cond(housesUpd)
      } yield housesUpd
    }
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
    def fill(houses: List[House], cond: List[House] => Boolean): Iterator[List[House]] = {
      val smokes: List[Smoke] = restOf(houses)
      for {
        comb <- smokes.permutations
        housesUpd = (houses.filter(_.smoke.isEmpty) zip comb).map { case (house, smoke) =>
          house.copy(smoke = Some(smoke))
        } ++ houses.filter(_.smoke.nonEmpty) if cond(housesUpd)
      } yield housesUpd
    }
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

  // The Englishman lives in the red house
  val condRedEng: PartialFunction[House, Boolean] =
    {
      case House(_, Some(Red), Some(resident), _, _, _) => resident == Englishman
      case House(_, Some(color), Some(Englishman), _, _, _) => color == Red
    }

  // The Spaniard owns the dog
  val condSpnDog: PartialFunction[House, Boolean] =
    {
      case House(_, _, Some(Spaniard), Some(animal), _, _) => animal == Dog
      case House(_, _, Some(resident), Some(Dog), _, _) => resident == Spaniard
    }

  // Coffee is drunk in the green house
  val condCoffGrn: PartialFunction[House, Boolean] =
    {
      case House(_, Some(Green), _, _, Some(drink), _) => drink == Coffee
      case House(_, Some(color), _, _, Some(Coffee), _) => color == Green
    }

  // The Ukrainian drinks tea
  val condUkrTea: PartialFunction[House, Boolean] =
    {
      case House(_, _, Some(Ukrainian), _, Some(drink), _) => drink == Tea
      case House(_, _, Some(resident), _, Some(Tea), _) => resident == Ukrainian
    }

  // The green house is immediately to the right of the ivory house
  val condGrnIvo: PartialFunction[(House, House), Boolean] =
    {
      case (House(i, Some(Green), _, _, _, _), House(j, Some(Ivory), _, _, _, _)) => i == j + 1
      case (House(i, Some(Ivory), _, _, _, _), House(j, Some(Green), _, _, _, _)) => j == i + 1
    }

  // The Old Gold smoker owns snails
  val condOGSnl: PartialFunction[House, Boolean] =
  {
    case House(_, _, _, Some(animal), _, Some(TheOldGold)) => animal == Snails
    case House(_, _, _, Some(Snails), _, Some(smoke)) => smoke == TheOldGold
  }

  // Kools are smoked in the yellow house
  val condKlYl: PartialFunction[House, Boolean] =
  {
    case House(_, Some(color), _, _, _, Some(Kools)) => color == Yellow
    case House(_, Some(Yellow), _, _, _, Some(smoke)) => smoke == Kools
  }

  // The man who smokes Chesterfields lives in the house next to the man with the fox
  val condCheFox: PartialFunction[(House, House), Boolean] =
  {
    case (House(i, _, _, _, _, Some(Chesterfields)), House(j, _, _, Some(Fox), _, _)) => (j - i).abs == 1
    case (House(i, _, _, Some(Fox), _, _), House(j, _, _, _, _, Some(Chesterfields))) => (j - i).abs == 1
  }

  // Kools are smoked in the house next to the house where the horse is kept
  val condKlHrs: PartialFunction[(House, House), Boolean] =
  {
    case (House(i, _, _, _, _, Some(Kools)), House(j, _, _, Some(Horse), _, _)) => (j - i).abs == 1
    case (House(i, _, _, Some(Horse), _, _), House(j, _, _, _, _, Some(Kools))) => (j - i).abs == 1
  }

  // The Lucky Strike smoker drinks orange juice
  val condLSOrg: PartialFunction[House, Boolean] =
  {
    case House(_, _, _, _, Some(drink), Some(LuckyStrike)) => drink == OrangeJuice
    case House(_, _, _, _, Some(OrangeJuice), Some(smoke)) => smoke == LuckyStrike
  }

  // The Japanese smokes Parliaments
  val condJpPr: PartialFunction[House, Boolean] =
  {
    case House(_, _, Some(resident), _, _, Some(Parliaments)) => resident == Japanese
    case House(_, _, Some(Japanese), _, _, Some(smoke)) => smoke == Parliaments
  }

  val cond: Function[House, Boolean] = (house: House) =>
    Seq(
      condRedEng, condSpnDog, condCoffGrn, condUkrTea,
      condOGSnl, condKlYl, condLSOrg, condJpPr
    ).view.foldLeft(true) { case (res, pf) =>
      res && pf.applyOrElse(house, (_: House) => true)
    }

  val condBoth: Function[(House, House), Boolean] = (hs: (House, House)) =>
    Seq(
      condGrnIvo, condCheFox, condKlHrs
    ).view.foldLeft(true) {
      case (res, pf) => res && pf.applyOrElse((hs._1, hs._2), (_: (House, House)) => true)
    }

  def filledAllCond(houses: List[House]): Boolean = {
    houses.foldLeft(true) { case (res: Boolean, house: House) =>
      res && cond(house)
    } && houses.combinations(2).foldLeft(true) {
      case (res: Boolean, List(h1, h2)) => res && condBoth(h1, h2)
      case _ => true
    }
  }

  /** Find all houses in the Einstein's Puzzle
   * @return house resident who drinks water and house resident who owns zebra
   * */
  def solve: Option[Solution] = {
    // 1. There are five houses
    // house0, ..., house4
    val houses: mutable.ListBuffer[House] = mutable.ListBuffer.from(List.range(0, 5).map(i => House.empty(i)))

    // Milk is drunk in the middle house
    houses(2) = houses(2).copy(drink = Some(Milk))
    // The Norwegian lives in the first house
    houses(0) = houses.head.copy(resident = Some(Norwegian))
    // The Norwegian lives next to the blue house
    houses(1) = houses(1).copy(color = Some(Blue))

    val solutions: List[List[House]] = for {
      housesWithColor <- {val l1 = Color.fill(houses.toList, filledAllCond).toList; println(s"count with color = ${l1.length}"); l1}
      housesWithResident <- {val l1 = Resident.fill(housesWithColor, filledAllCond).toList; println(s"count with resident = ${l1.length}"); l1}
      housesWithAnimal <- {val l1 = Animal.fill(housesWithResident, filledAllCond).toList; println(s"count with animal = ${l1.length}"); l1}
      housesWithDrink <- {val l1 = Drink.fill(housesWithAnimal, filledAllCond).toList; println(s"count with drink = ${l1.length}"); l1}
      housesWithSmoke <- {val l1 = Smoke.fill(housesWithDrink, filledAllCond).toList; println(s"count with smoke = ${l1.length}"); l1.foreach(House.print); l1}
    } yield housesWithSmoke

    println(s"Solutions count = ${solutions.length}")
    solutions.foreach { houses => House.print(houses) }

    solutions.headOption.flatMap { list =>
      val waterDrinker: Option[Resident] = list.find(_.drink.contains(Water)).flatMap(_.resident)
      val zebraOwner: Option[Resident] = list.find(_.animal.contains(Zebra)).flatMap(_.resident)
      (waterDrinker zip zebraOwner).map { case (x, y) =>
        Solution(x, y)
      }
    }
  }
}
