package org.lanyard.dist.disc.markov

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.dist.disc.Discrete
import org.lanyard.random.RNG

class HoMarkov[A]( val order: Int, probabilities: Array[Array[Double]] )( implicit disc: Discrete[A] )
  extends Distribution[Array[A]] {

  import disc._

  require( probabilities.forall( _.length == disc.size ), 
    "HoMarkov parameter probabilities' columns do not equal size of discrete. Found values: " + probabilities.map( _.length ).mkString(" ") )

  // cumulative offsets
  private val offsets = Array.tabulate( order )( math.pow( disc.size, _).toInt ).scanLeft(0)( _ + _ )

  require( probabilities.length == offsets.sum + math.pow( disc.size, order ),
    "HoMarkov chain parameter probabilities has not enough rows. Found value: " + probabilities.length )

  private val probs = probabilities.map( _.clone )

  def apply( value: Array[A]): Double = 
    if( value.length != 0 ) {
      var hashOfPrecursors = asInt( value( 0 ) )
      var like = probabilities(0)( hashOfPrecursors )

      var pos = 1
      while( pos < value.length ) {
        val element = asInt( value( pos ) )
        val startOfProbs = offsets(math.min(pos, order))
        like *= probabilities( startOfProbs + hashOfPrecursors )( element )

        // compute hash of the precursor to access the matrix
        if( pos <= order - 1) {
          // add new most significant bits
          hashOfPrecursors += (element * math.pow( size, pos )).toInt
        } else {
          // delete bits of least significant element using shift
          // add new most significant bits
          hashOfPrecursors = (hashOfPrecursors >>> bitsPerSymbol) + (element * math.pow( size, order - 1)).toInt
        }
        pos += 1
      }
      like
    } else {
      0.0
    }
 
  def random( source: RNG ): (Array[A], RNG) = ???
}

object HoMarkov {

  def apply[A]( order: Int ): HoMarkov[A] = ???

}
