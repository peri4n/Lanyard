package org.lanyard.dist.disc.markov

import org.lanyard.dist.Distribution
import org.lanyard.dist.disc.Discrete
import org.lanyard.random.RNG

/** Inhomogeneous discrete markov models. */
class InMarkov[A]( length: Int, order: Int, probs: Array[Array[Double]] )( implicit disc: Discrete[A] ) extends Markov[A](length, order, probs) {

  import math._

  /** Cummulative offsets */
  private val probOffsets: Array[Int] = Array.tabulate( length )( i => 
    if( i <= order ) 
      pow(disc.size, i).toInt 
    else 
      pow( disc.size, order).toInt 
  ).scanLeft(0)(_ + _)

  def offset( position: Int ): Int = probOffsets( position )

}

object InMarkov {

  def apply[A]( length: Int, order: Int, probs: Array[Array[Double]] )( implicit disc: Discrete[A] ): InMarkov[A] =
    new InMarkov[A]( length, order, probs.map( _.clone ) )

}
