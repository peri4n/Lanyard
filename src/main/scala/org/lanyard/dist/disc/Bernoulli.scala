package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import scala.annotation.switch

/** The Bernoulli distribution is a discrete probability distribution
  * of a success in a single experiment. 
  * 
  * @constructor Creates a Bernoulli distribution
  * @param p probability of success
  */
case class Bernoulli( p: Double ) extends Distribution[Boolean] {

  import math._

  /** Computes the mean. */
  def mean = p

  /** Computes the variance. */
  def variance = p * ( 1 - p )

  /** Computes the skewness. */
  def skewness = (( 1 - p ) - p ) / sqrt( p * ( 1 - p ) )

  /** Computes the probability of a bernoulli experiment outcome.
    * 
    * @param value outcome of the experiment
    * @return probability of the outcome
    */
  override def apply( value: Boolean ): Double = (value: @switch) match {
    case true => p
    case _ => ( 1 - p )
  }

  /** Draws a random value from the distribution.
    * 
    * @param source a random number generator
    * @return a pair of a random value and the updated generator
    */
  def random( source: RNG ): (Boolean, RNG) = {
    val (doub, nextRNG) = source.nextDouble
    (doub > p, nextRNG)
  }

}
