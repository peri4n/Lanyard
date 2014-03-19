package org.lanyard.dist.disc.markov

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.dist.disc.Discrete
import org.lanyard.util.Util
import org.lanyard.random.RNG
import scala.collection.mutable.ListBuffer

/**
 * Homogeneous discrete markov models are probability distributions that allow the modeling of sequential data of arbitrary length.
 *
 * @constructor Creates a homogeneous discrete markov model of given order.
 *
 * @param length length of the sequences which can be drawn from this distribution
 * @param order order of the markov model
 */
class HoMarkov[A] private[markov] (length: Int, order: Int, probs: Array[Array[Double]])(implicit disc: Discrete[A])
  extends Markov[A](length, order, probs) {

  import math._

  require(probs.forall(_.length == disc.size),
    "HoMarkov parameter probabilities' columns do not equal size of discrete. Found values: " + probs.map(_.length).mkString(" "))

  /** Cummulative offsets */
  private val probOffsets: Array[Int] = Array.tabulate(order)(pow(disc.size, _).toInt).scanLeft(0)(_ + _)

  require(probs.length == probOffsets.sum + pow(disc.size, order),
    "HoMarkov chain parameter probabilities has not enough rows. Found rows: " + probs.length)

  def offset( position: Int ): Int = probOffsets( min( position, order) )

}

object HoMarkov {

  def apply[A](length: Int, order: Int, probabilities: Array[Array[Double]])(implicit disc: Discrete[A]): HoMarkov[A] =
    new HoMarkov[A](length, order, probabilities.map(_.clone))(disc)

}
