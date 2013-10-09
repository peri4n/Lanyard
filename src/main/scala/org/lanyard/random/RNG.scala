package org.lanyard.random

import scala.annotation.tailrec

/** (Pseudo) Random Number Generators deterministicly produce seemingly random bits.
  * RNGs are designed to be immutable, so every invocation has to not only return a
  * random primitive type but also a new state of the RNG to produce further draws.
  */
trait RNG {

  /** Abstract method to compute random 64 bits stored in a `Long`.
    * 
    * @return pair of a random `Long` an the updated state of the RNG
    */
  def nextLong: (Long, RNG)

  /** Computes random 32 bits stored in an `Int`.
    * 
    * @return pair of a random `Int` an the updated state of the RNG
    */
  def nextInt: (Int, RNG) = {
    val (l, rng) = nextLong
    ((l >>> 16).toInt, rng)
  }

  /** Computes a random double-precision floating point number.
    * 
    * @return pair of a random `Double` and the updated state of the RNG
    */
  def nextDouble: (Double, RNG) = {
    val (l, rng) = nextLong
    // -9.223372036854776E18 == Long.MIN_VALUE.toDouble
    // 5.42101086242752217E-20 == 1 / ( Long.MAX_VALUE.toDouble - Long.MIN_VALUE.toDouble);
    ( math.abs( l ) / Long.MaxValue.toDouble, rng)
  }

  /** Computes a random bit.
    * 
    * @return pair of a random `Boolean` and the updated state of the RNG
    */
  def nextBoolean: (Boolean, RNG) = {
    val (l, rng) = nextLong
    ((l & 1L) == 1L, rng) // if lsb is set
  }

  /** Skips a given number of draws. This is helpful to discard the first few
    * samples which are often not sufficiently random.
    * 
    * @return updated RNG
    */
  @tailrec
  final def forward(steps: Int): RNG = steps match {
    case 0 => this
    case x => {
      val (long, rng) = nextLong
      rng.forward( x - 1 )
    }
  }

}

object RNG {

  def toRandom[A]( implicit ev: Random[A] ): Random[A] = ev

}
