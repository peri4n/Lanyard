package org.lanyard.util

import org.lanyard.random.RNG
import scala.annotation.tailrec

object Util {

  import math._

  /** Draws an index using the given probabilities. 
    * 
    * @param source source of randomness
    * @param probs probabilities
    * @return random index
    */
  def drawFrom( source: RNG, probs: Array[Double] ): (Int, RNG) = {
    require( abs( probs.sum - 1) <= 1E-10 , "Probabilities in array didn't sum to one.")

    val ( d, rng) = source.nextDouble
    @tailrec def draw( probs: Array[Double], idx: Int, sum: Double): Int = {
      if( d <= probs(idx) + sum ) {
        idx
      } else {
        draw( probs, idx + 1, sum + probs(idx) )
      }
    }

    (draw( probs, 0, 0.0 ), rng)
  }
}
