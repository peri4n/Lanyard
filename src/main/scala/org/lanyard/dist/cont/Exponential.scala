package org.lanyard.dist.cont

import org.lanyard.dist.DistFactory
import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/** The Exponential distribution is a continuous probability distribution.
  * 
  * @constructor Create an Exponential distribution
  * @param lambda lambda parameter
  */
case class Exponential(lambda: Double) extends Distribution[Double] {

  require(lambda > 0, "Exponential distribution parameter lambda needs to be strictly positive. Found value: " + lambda)

  import math._

  type Parameter = Double

  /** Mean of the beta distribution */
  def mean: Double = 1 / lambda

  /** Variance of the beta distribution */
  def variance: Double = 1 / (lambda * lambda)

  /** Precomputes the logarithm of lambda for faster computation. */
  private val logLambda = log(lambda)

  /**
   * Computes the logarithm of the probability density function.
   *
   * @param value Value to compute the probability for
   * @return logarithm of the probability
   */
  override def logLike(value: Double): Double =
    if (value >= 0)
      logLambda + (-logLambda * value)
    else
      Double.NegativeInfinity

  /**
   * Draws a random sample from this exponential distribution.
   *
   * @param source source of randomness
   * @return pair of an exponential sample and the updated RNG
   */
  def random(source: RNG): (Double, RNG) = {
    val (d, rng) = source.nextDouble
    (-log(d) / lambda, rng)
  }
}

object Exponential {

  /** Distribution factory of the exponential distribution */
  implicit object ExponentialDistFactory extends DistFactory[Exponential] {

    /** Create an exponential distribution
     *
     * @param lambda lambda parameter
     * @return exponential distribution with given lambda
     */
    def create(lambda: Double) = new Exponential(lambda)
  }

}
