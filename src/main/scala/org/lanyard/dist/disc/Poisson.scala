package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Poisson(lambda: Double) extends Distribution[Int] {

  require(0 < lambda, s"Poisson distribution parameter lambda needs to be strictly positive. Found value: ${lambda}")

  import math._

  /** The following variables are used by the PTRS algorithm for sampling. */
  val b = 0.931 + 2.53 * sqrt(lambda)
  val a = -0.059 + 0.02483 * b
  val vr = 0.9277 - 3.6224 / (b - 2)
  val invAlpha = 1.1239 + 1.1328 / (b - 3.4)
  val logLambda = log(lambda)

  /** Computes the probability of a value under this distribution. */
  override def apply(value: Int): Double = if (value < 0) 0 else 0

  /** the mean of this distribution. */
  def mean: Double = lambda

  /** the variance of this distribution. */
  def variance: Double = lambda

  /**
   * Generates a random variate from this poisson distribution.
   *
   * @param source a random number generator
   * @return a pair with the random variate and the updated generator
   */
  def random(source: RNG): (Int, RNG) = if (lambda < 10) simpleDraw(source) else ptrs(source)

  /**
   * Generates a random variate from this poisson distribution.
   *
   * This method is invoked only if the parameter lambda of the
   * distribution is small ( < 10 ).  The algorithm used is described
   * in "Knuth, D. 1969. 'Seminumerical Algorithms. The Art of
   * Computer Programming' vol 2.
   *
   * @param source source of randomness
   * @return random poisson variate together with updated generator
   */
  private def simpleDraw(source: RNG): (Int, RNG) = {
    val L = exp(-lambda)
    var k = 0
    var p = 1
    var rng: RNG = source
    do {
      k += 1
      val (uni, nextRng) = rng.nextDouble
      p *= uni
      rng = nextRng
    } while (p > L)
    (k - 1, rng)
  }

  /**
   * Generates a random variate from this poisson distribution.
   *
   * This method is invoked only if the parameter lambda of this
   * distribution is big enough ( >= 10 ).  The algorithm used is
   * described in "Hörmann, W. 1992. 'The Transformed Rejection
   * Method for Generating Poisson Random Variables'.
   *
   * @param source source of randomness
   * @return random poisson variate together with the updated generator
   */
  private def ptrs(source: RNG): (Int, RNG) = {
    var rng = source
    var us = 0.0
    var k = 0
    do {
      /** STEP 1 */
      val (u, rng1) = rng.nextDouble
      val (v, rng2) = rng1.nextDouble
      rng = rng2
      us = 0.5 - abs(u - 0.5)
      k = floor((2 * a / us + b) * (u - 0.5) + lambda + 0.43).toInt
    } while ((us < 0.07 || v > vr)  /** STEP 2 */
      && (k < 0 || (us < 0.013 && v > us)
        || log(v * invAlpha / (a / (us * us) + b)) /** STEP 3 */
        > -lambda + k * logLambda - LogFactorial(k)))
    (k, rng)
  }
}
