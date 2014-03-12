package org.lanyard.dist.cont

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/** The continuous Uniform distribution. It assigns equal probabilities to values in a continuous range.
  * 
  * @constructor Creates a continuous uniform distribution
  * @param leftLimit left end of the support
  * @param rightLimit right end of the support
  */
case class Uniform( leftLimit: Double, rightLimit: Double ) extends Distribution[Double] {

  require( leftLimit <= rightLimit,
    "Continuous uniform distribution parameter leftLimit has to be smaller or equal than rightLimit. " + 
      "Found values: [" + leftLimit + ", " + rightLimit + " ]" )

  /** Computes the probability. */
  override def apply( value: Double ): Double = 
    if( leftLimit <= value && value <= rightLimit ) 1.0 / ( rightLimit - leftLimit ) else 0.0

  /** Draws a random value from this uniform distribution.
    * 
    * @param source a random number generator
    * @return pair of a random value and the updated generator
    */
  def random( source: RNG ): ( Double, RNG ) = {
    val ( draw, nextRNG ) = source.nextDouble
    ( draw * ( rightLimit - leftLimit ) + leftLimit, nextRNG )
  }
}
