package org.lanyard.dist.cont

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

/** The chi squared distribution
  * 
  * @constructor Create a chi squared distribution with given degrees of freedom
  * @param dgf degrees of freedom
  */
class ChiSquared( val dgf: Int ) extends Distribution[Double] {

  import math._

  private lazy val constantLogTerm = - dgf.toDouble / 2 * log( 2 ) - LogGamma( dgf.toDouble / 2 )

  private lazy val gamma = Gamma( dgf.toDouble / 2, 0.5)

  def apply( value: Double): Double = exp( logLike( value ) )

  override def logLike( value: Double): Double = constantLogTerm + ( dgf.toDouble / 2 - 1) * log( value ) - ( value / 2 )

  def random( source: RNG ): (Double, RNG) = gamma.random( source )
}
