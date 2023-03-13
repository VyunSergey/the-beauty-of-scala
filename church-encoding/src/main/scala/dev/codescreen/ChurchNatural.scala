package dev.codescreen

import dev.codescreen.ChurchNatural._

/**
 * This is a fundamental example of polymorphism usage
 * and a basic example of tagless final encoding
 *
 */
trait ChurchNatural {
  // self reference needed to check additional typing requirements
  // and to refer this object in the subexpressions which have their own `this`
  self =>

  /**
   * each number accepts initial value and the incrementing function
   * since type argument `N` is erased and we restrict side-effects of the methods
   * the only sensible thing this method can do is to apply the increment function starting from the zero argument
   * amount of this applications encodes the value of the number
   *
   * @param zero       initial value
   * @param increment  step function
   */
  def count[N](zero: N)(increment: N => N): N

  def successor: ChurchNatural = new ChurchNatural {
    def count[N](zero: N)(increment: N => N): N = increment(self.count(zero)(increment))
  }

  /** soft -1 return 0 if number is already zero */
  def predecessor: ChurchNatural = count((Zero, Zero)) { case (n, _) => (n.successor, n) }._2

  def toInt: Int = ???
  def isZero: Boolean = ???
  def +(that: ChurchNatural): ChurchNatural = ???
  def -(that: ChurchNatural): ChurchNatural = ???
  def *(that: ChurchNatural): ChurchNatural = ???
  def exp(that: ChurchNatural): ChurchNatural = ???
  def ===(that: ChurchNatural): Boolean = ???
  def <(that: ChurchNatural): Boolean = ???
}

object ChurchNatural {
  def fromInt(i: Int): Option[ChurchNatural] = ???

  val Zero: ChurchNatural = new ChurchNatural {
    def count[N](zero: N)(increment: N => N): N = zero
  }

  val One: ChurchNatural = Zero.successor
  val Two: ChurchNatural = One.successor
}
