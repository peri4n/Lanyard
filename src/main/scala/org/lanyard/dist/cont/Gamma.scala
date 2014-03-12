package org.lanyard.dist.cont

import org.lanyard.LogLike
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma
import scala.annotation.tailrec

/**
 * The gamma distribution is a continuous probability density distribution. It is defined for strictly positive values.
 * For an explanation see [[http://en.wikipedia.org/wiki/Gamma_distribution wikipedia]].
 *
 * @constructor Creates a gamma distribution with the given parameters.
 * @param shape shape parameter of the gamma distribution
 * @param scale scale parameter of the gamma distribution
 */
class Gamma private (val shape: Double, val scale: Double, private val shifted: Boolean) extends Distribution[Double] {

  import Gamma._
  import math._

  require(shape > 0.0, "Gamma distribution parameter shape needs to be strictly positive. Found value: " + shape)
  require(scale > 0.0, "Gamma distribution parameter scale needs to be strictly positive. Found value: " + scale)

  /** Logarithm of the constant term of the gamma distribution */
  private lazy val constantLogTerm = -LogGamma(shape) - shape * log(scale)

  /** Computes the rate parameter. */
  def rate = 1 / scale

  /** Computes the mean. */
  def mean = shape * scale

  /** Computes the variance. */
  def variance = shape * scale * scale

  /** Computes the skewness. */
  def skewness = 2 / math.sqrt(shape)

  /** Computes the kurtosis. */
  def kutorsis = 6 / shape

  /**
   * Computes the logarithm of the probability density function.
   *
   * @param value value to compute the log pdf for
   * @return value of the log pdf
   */
  override def logLike(value: Double): LogLike = constantLogTerm + scale * log(value) - (value / scale)

  /**
   * Draws a random number from this gamma distribution.
   * The algorithm used is adopted from
   * ''Marsaglia, G. and Tsang W.-W. 2000. "A Simple Method for Generating Gamma Variables",
   * ACM Transactions on Mathematical Software, vol. 26, no. 3, pp. 363-372''
   *
   * @param source source of randomness
   * @return pair of the draw and the updated RNG
   */
  @tailrec final def random(source: RNG): (Double, RNG) = {
    val a1 = shape - 1.0 / 3.0
    val a2 = 1.0 / sqrt(9.0 * a1)

    @tailrec def useGaussian(s: RNG): (Double, Double, RNG) = {
      val (x, rng) = standardGaussian.random(s)
      val v = 1.0 + a2 * x
      if (v <= 0.0) useGaussian(rng) else (x, v, rng)
    }
    var (x, v, rng1) = useGaussian(source)
    v = v * v * v
    val (u, rng2) = rng1.nextDouble
    if (u > 1.0 - 0.331 * x * x * x * x && log(u) > 0.5 * x * x + a1 * (1.0 - v + log(v)))
      random(rng2)
    else {
      if ( shifted ) {
        val (u, rng3) = rng2.nextDouble
        (math.pow(u, 1.0 / (shape - 1)) * a1 * v * scale, rng3)
      } else {
        (a1 * v * scale, rng2)        
      }
    }
  }

  /** Converts the object to a string. */
  override def toString: String = s"Gamma( shape = ${shape}, scale = ${scale} )"
}

object Gamma {

  /** Standard normal distribution. Used for sampling purposis. */
  private val standardGaussian = Gaussian()

  /**
   * Create a gamma distribution.
   *
   * @param shape shape parameter of the gamma distribution
   * @param scale scale parameter of the gamma distribution
   * @return gamma distribution with given parameter
   */
  def apply(shape: Double, scale: Double): Gamma =
    if (shape < 1.0)
      new Gamma(shape + 1, scale, true)
    else
      new Gamma(shape, scale, false)

}
