package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/** This uniform distribution is a discrete probability
  * distribution. It assigns a probability to the numbers in the range
  * [leftLimit, rightLimit] the same probability. 
  * 
  * @constructor Create a discrete uniform distribution.
  * @param leftLimit left end of the support
  * @param rightLimit right end of the support
*/
case class Uniform( leftLimit: Int, rightLimit: Int) extends Distribution[Int] {

  require( leftLimit <= rightLimit, 
    "Discrete uniform distribution parameters leftLimit and rightLimit have to form a valid interval. Found value: [" + 
      leftLimit + ", " + rightLimit + " ]" )

  private val size = rightLimit - leftLimit + 1

  /** Computes the probability of a value. */
  override def apply( value: Int ): Double = 1.0 / size

  /** Computes the mean of the uniform distribution. */
  def mean: Double = ( rightLimit + leftLimit ).toDouble / 2

  /** Computes the variance of the uniform distribution. */
  def variance: Double = ( size * size - 1.0 ) / 12

  /** Draws a random value from the distribution.
    * @param source a random number generator
    * @return a pair of a random draw and the updated generator
    */
  def random( source: RNG ): (Int, RNG) = {
    val (draw, nextRNG) = source.nextInt
    ( (draw % size) + leftLimit, nextRNG )
  }
}
