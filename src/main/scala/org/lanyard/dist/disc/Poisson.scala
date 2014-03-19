package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Poisson(lambda: Double) extends Distribution[Int] {

  require(0 < lambda, s"Poisson distribution parameter lambda needs to be strictly positive. Found value: ${lambda}")

  import math._

  /** Computes the probability of a value under this distribution. */
  override def apply(value: Int): Double = if (value < 0) 0 else 0

  /** the mean of this distribution. */
  def mean: Double = lambda

  /** the variance of this distribution. */
  def variance: Double = lambda

  /**
   * Generates a random variate from this distribution.
   *
   * @param source a random number generator
   * @return a pair with the random variate and the updated generator
   */
  def random(source: RNG): (Int, RNG) = ???
}
