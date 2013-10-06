package org.lanyard.random

/** (Pseudo) Random Number Generators deterministicly produce seemingly random bits.
  * RNGs are designed to be immutable, so every invocation has to not only return a
  * random primitive type but also a new state of the RNG to produce further draws.
  */
trait RNG {

  /** Computes random 64-bits stored in a `Long`.
    * 
    * @return Pair of a random `Long` an the updated state of the RNG
    */
  def nextLong: (Long, RNG)

  /** Computes random 32-bits stored in an `Int`.
    * 
    * @return Pair of a random `Int` an the updated state of the RNG
    */
  def nextInt: (Int, RNG) = {
    val (l, rng) = nextLong
    ((l >>> 16).toInt, rng)
  }

  /** Computes a random double-precision floating point number.
    * 
    * @return Pair of a random `Double` and the updated state of the RNG
    */
  def nextDouble: (Double, RNG) = {
    val (l, rng) = nextLong
    // -9.223372036854776E18 == Long.MIN_VALUE.toDouble
    // 5.421010862427522E-20 == 1 / ( Long.MAX_VALUE.toDouble - Long.MIN_VALUE.toDouble);
    val double = ( l.toDouble - -9.223372036854776E18 ) * 5.421010862427522E-20
    if( 0.0 < double && double < 1 ) (double, rng) else rng.nextDouble
  }

  /** Computes a random bit.
    * 
    * @return Pair of a random `Boolean` and the updated state of the RNG
    */
  def nextBoolean: (Boolean, RNG) = {
    val (l, rng) = nextLong
    ((l & 1L) == 1L, rng) // if lsb is set
  }

}
