package org.lanyard.dist.cont

import org.lanyard.dist.DistFactory
import org.lanyard.LogLike
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import scala.annotation.tailrec

/**
 * The univariate Gaussian (or normal) distribution is a continous probability distribution.
 * Its support is the entire real axis.
 *
 * @constructor Creates a gaussian distribution.
 * @param mean mean of the distribution
 * @param variance variance of the distribution
 */
case class Gaussian(mean: Double = 0, variance: Double = 1) extends Distribution[Double] {

  import math._

  require(variance > 0, "Normal distribution parameter variance needs to be strictly positive. Found value: " + variance)

  type Parameter = (Double, Double)

  /** Computes the standard derivation. */
  def stdDeviation = sqrt(variance)

  /** Computes the skewness. */
  def skewness = 0.0

  /** Computes the kurtosis. */
  def kurtosis = 0.0

  /**
   * Computes the probability density function.
   *
   * @param value value to compute the pdf for
   * @return pdf of value
   */
  def apply(value: Double): Double = exp(logLike(value))

  /** Computes the logarithm of the probability density function. */
  override def logLike(value: Double): Double = {
    0.0
  }

  /**
   * Draws a random number from this gaussian distribution.
   * The algorithm used is adopted from
   *  ''Leva, J. L. 1992. "A Fast Normal Random Number Generator", ACM Transactions on Mathematical Software, vol. 18, no. 4, pp. 449-453''
   *
   * @param source source of randomness
   * @return pair of the draw and the updated RNG
   */
  @tailrec final def random(source: RNG): (Double, RNG) = {
    val (draw1, rng1) = source.nextDouble
    val (draw2, rng2) = rng1.nextDouble
    val v = 1.7156 * (draw2 - 0.5)
    val x = draw1 - 0.449871
    val y = abs(v) + 0.386595
    val q = (x * x) + y * (0.19600 * y - 0.25472 * x)
    if (q > 0.27597 && (q > 0.27846 || (v * v) > -4.0 * log(draw1) * draw1 * draw1))
      random(rng2)
    else
      (mean + stdDeviation * v / draw1, rng2)
  }
}

object Gaussian {

  /** Distribution factory for the gaussian distribution. */
  implicit object GaussianDistFactory extends DistFactory[Gaussian] {

    /** Creates a gaussian distribution
     *
     * @param meanVariance pair of mean and variance
     * @return gaussian distribution with given mean and variance
     */
    def create(meanVariance: (Double, Double)) = new Gaussian(meanVariance._1, meanVariance._2)
  }

}
