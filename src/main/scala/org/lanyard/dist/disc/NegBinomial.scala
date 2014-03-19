package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class NegBinomial( nrOfFailures: Int, prob: Double) extends Distribution[Int] {

  require( 0 < nrOfFailures, s"Negative binomial distribution parameter nrOfFailures needs to be strictly positive. Found value: ${nrOfFailures}" )
  require( 0 < prob && prob < 1, s"Negative binomial distribution parameter prob needs to be in range (0,1). Found value: ${prob}")

  override def apply( value: Int ): Double = 0.0

  def mean: Double = ( nrOfFailures * prob ) / ( 1 - prob )

  def variance: Double =  ( nrOfFailures * prob ) / (( 1 - prob ) * ( 1 - prob))

  def random( source: RNG ): (Int, RNG) = ???

}
