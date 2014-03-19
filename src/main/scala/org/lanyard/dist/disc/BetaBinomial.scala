package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class BetaBinomial( alpha: Double, beta: Double ) extends Distribution[Int] {

  require( 0 < alpha, s"The Beta binomial distribution parameter alpha should be strictly positive. Found value: ${alpha}" )
  require( 0 < beta, s"The Beta binomial distribution parameter beta should be strictly positive. Found value: ${beta}" )

  override def apply( value: Int ): Double = 0.0 /** support is 0 <= k <= n */

  def random( source: RNG ): (Int, RNG) = ???

}
