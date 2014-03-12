package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/** @TODO generalize so that the weights don't have to sum to one. */
case class Categorial[A]( weights: Seq[Double] )( implicit disc: Discrete[A] ) extends Distribution[A] {

  import disc._

  require( weights.length == disc.size, "Categorial distribution parameter weights has to have the same length as the size of the associated discrete alphabet." )

  private val cummulative = weights.scanLeft( 0.0 )( _ + _ ).toArray

  override def apply( value: A ): Double = cummulative( asInt( value ) + 1 ) - cummulative( asInt( value ) )

  def random( source: RNG ): ( A, RNG ) = {
    val ( doub, rng ) = source.nextDouble
    var i = 1
    while ( doub < cummulative( i ) ) {
      i += 1
    }
    ( fromInt( i ), rng )
  }
}
