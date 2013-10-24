package org.lanyard.dist.disc.markov

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.dist.disc.Discrete
import org.lanyard.random.RNG
import scala.reflect.ClassTag

/** Homogeneous discrete markov models are probability mass functions that allow the modeling of sequential data of arbitrary length.
  * 
  * @constructor Create a homogeneous discrete markov model of given order.
  * 
  * @param length length of the sequences which can be drawn from this distribution
  * @param order order of the markov model
  */
class HoMarkov[A]( val length: Int, val order: Int, private val probabilities: Array[Array[Double]] )( implicit disc: Discrete[A], tag: ClassTag[A] )
  extends Distribution[Array[A]] {

  import disc._
  import math._

  require( probabilities.forall( _.length == disc.size ),
    "HoMarkov parameter probabilities' columns do not equal size of discrete. Found values: " + probabilities.map( _.length ).mkString(" ") )

  /** Cummulative offsets */
  private val offsets = Array.tabulate( order )( pow( disc.size, _).toInt ).scanLeft(0)( _ + _ )

  require( probabilities.length == offsets.sum + pow( disc.size, order ),
    "HoMarkov chain parameter probabilities has not enough rows. Found value: " + probabilities.length )

  def apply( value: Array[A] ): Double = exp( logLike( value ) )

  /** Computes the log probability mass function.
    * 
    * @param value sequence of discrete elements
    * @return log likelihood of the sequence
    */
  override def logLike( value: Array[A]): LogLike =
    if( value.length != 0 ) {
      var hashOfPrecursors = asInt( value( 0 ) )
      var like = log( probabilities(0)( hashOfPrecursors ) )

      var pos = 1
      while( pos < value.length ) {
        val element = asInt( value( pos ) )
        val startOfProbs = offsets( min(pos, order))
        like += log( probabilities( startOfProbs + hashOfPrecursors )( element ) )

        // compute hash of the precursor to access the matrix
        if( pos <= order - 1) {
          // add new most significant bits
          hashOfPrecursors += (element * pow( size, pos )).toInt
        } else {
          // delete bits of least significant element using shift
          // add new most significant bits
          hashOfPrecursors = (hashOfPrecursors >>> bitsPerSymbol) + (element * pow( size, order - 1)).toInt
        }
        pos += 1
      }
      like
    } else {
      Double.NegativeInfinity
    }
 
  def random( source: RNG ): (Array[A], RNG) = {
    val seq = new Array[A]( length )


    (seq, null)
  }
}

object HoMarkov {

  def apply[A]( order: Int ): HoMarkov[A] = ???

}
