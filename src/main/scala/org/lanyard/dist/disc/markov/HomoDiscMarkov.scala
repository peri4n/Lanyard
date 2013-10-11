package org.lanyard.dist.disc.markov

import org.lanyard._
import org.lanyard.dist.Distribution

class HomoDiscMarkov[A] private( val order: Int, val probabilities: Array[Double])(implicit disc: Discrete[A]) 
  extends Distribution[Array[A]] {

  def apply(value: A): Prob = {
    0.0
  }
}

object HomoDiscMarkov {

  def apply[A]( order: Int ): HomoDiscMarkov[A] = {
    null
  }

}
