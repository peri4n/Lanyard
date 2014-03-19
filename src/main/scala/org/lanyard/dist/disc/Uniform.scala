package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/**
 * This uniform distribution is a discrete probability
 * distribution. It assigns a probability to the numbers in the range
 * [left, right] the same probability.
 *
 * @constructor Create a discrete uniform distribution.
 * @param left left end of the support
 * @param right right end of the support
 */
case class Uniform(left: Int, right: Int) extends Distribution[Int] {

  require(left <= right,
    "Discrete uniform distribution parameters left and right have to form a valid interval. Found value: [" +
      left + ", " + right + " ]")

  private val size = right - left + 1

  /** Computes the probability of a value. */
  override def apply(value: Int): Double = if( left <= value && value <= right) 1.0 / size else 0.0

  /** Computes the mean of the uniform distribution. */
  def mean: Double = (right + left).toDouble / 2

  /** Computes the variance of the uniform distribution. */
  def variance: Double = (size * size - 1.0) / 12

  /**
   * Draws a random value from the distribution.
   * @param source a random number generator
   * @return a pair of a random draw and the updated generator
   */
  def random(source: RNG): (Int, RNG) = {
    val (draw, nextRNG) = source.nextInt
    ((draw % size) + left, nextRNG)
  }
}
