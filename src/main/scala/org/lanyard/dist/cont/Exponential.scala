package org.lanyard.dist.cont

import org.lanyard.dist.DistFactory
import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Exponential(lambda: Double) extends Distribution[Double] {

  require(lambda > 0, "Exponential distribution parameter lambda needs to be strictly positive. Found value: " + lambda)

  type Parameter = Double

  /** Mean of the beta distribution */
  val mean = 1 / lambda

  /** Variance of the beta distribution */
  val variance = 1 / (lambda * lambda)

  /** Precomputes the logarithm of lambda for faster computation. */
  private val logLambda = math.log(lambda)

  /**
   * Computes the logarithm of the probability density function.
   *
   * @param value Value to compute the probability for
   * @return logarithm of the probability
   */
  def apply(value: Double): LogLike =
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
    (-math.log(d) / lambda, rng)
  }
}

object Exponential {

  /** Distribution factory of the exponential distribution */
  implicit object ExponentialDistFactory extends DistFactory[Exponential] {

    /**
     * Create an exponential distribution
     *
     * @param lambda lambda parameter
     * @return exponential distribution with given lambda
     */
    def create(lambda: Double) = new Exponential(lambda)
  }

}
