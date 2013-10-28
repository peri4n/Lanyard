package org.lanyard.dist.cont

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

class ChiSquared( val dgf: Int ) extends Distribution[Double] {

  import math._

  private lazy val constantLogTerm = - dgf.toDouble / 2 * log( 2 ) + LogGamma( dgf.toDouble / 2 )

  private lazy val gamma = Gamma( dgf.toDouble / 2, 0.5)

  def apply( value: Double): Double = exp( logLike( value ) )

  def logLike( value: Double): Double = ???

  def random( source: RNG ): (Double, RNG) = gamma.random( source )
}
