package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

class Categorial[A]( weights: Array[Double] )(implicit disc: Discrete[A]) extends Distribution[A] {

  import disc._

  private val cummulative = weights.scanLeft(0.0)( _ + _ ).tail

  def apply( value: A): Double = weights( asInt( value ) )

  def random( source: RNG ): (A, RNG) = {
    val (doub, rng) = source.nextDouble
    var i = 0
    while( doub < cummulative(i) ) {
      i+= 1
    }
    (fromInt( i ), rng)
  }

}
