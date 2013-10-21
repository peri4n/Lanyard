package org.lanyard.dist.disc.markov

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

class HomoDiscMarkov[A] private( val order: Int, val probabilities: Array[Double] )( implicit disc: Discrete[A] ) 
  extends Distribution[Array[A]] {

  def apply( value: Array[A]): LogLike = ???

  def random( source: RNG ): (Array[A], RNG) = ???
}

object HomoDiscMarkov {

  def apply[A]( order: Int ): HomoDiscMarkov[A] = ???

}
