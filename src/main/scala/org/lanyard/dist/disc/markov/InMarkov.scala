package org.lanyard.dist.disc.markov

import org.lanyard.dist.Distribution
import org.lanyard.dist.disc.Discrete
import org.lanyard.random.RNG

class InMarkov[A]()( implicit disc: Discrete[A] ) extends Distribution[Array[A]] {

  import disc._

  def apply( value: Array[A] ): Double = 0.0

  def random( source: RNG ): (Array[A], RNG) = ???

}
