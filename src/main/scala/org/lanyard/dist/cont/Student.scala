package org.lanyard.dist.cont

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

/** Non-standardized Student distribution
  * 
  * @constructor Creates a student distribution
  * 
  * @param dgf degrees of freedom
  * @param mu location parameter
  * @param sigma shape parameter
  */
class Student( val dgf: Double, val mu: Double = 0.0, val sigma: Double = 1.0 ) extends Distribution[Double] {

  require( dgf > 0.0 , "Student distribution parameter dgf needs to be strictly positive. Found value: " + dgf )

  import math._

  private lazy val constantLogTerm = LogGamma( (dgf + 1) / 2 ) - LogGamma( dgf /2 )

  def apply( value: Double): Double = exp( logLike( value ) )

  override def logLike( value: Double): Double = constantLogTerm - 
    log( sqrt( Math.PI  * dgf ) * sigma ) -
    (( dgf + 1 ) / 2 ) * log( 1.0 +  ((value - dgf) / sigma) * ((value - dgf) / sigma) / dgf)

  def random( source: RNG ): (Double, RNG) = {
    val (u1, rng1) = source.nextDouble
    val (u2, rng2) = rng1.nextDouble
    val y = sqrt( dgf * ( pow( u1, 2.0 / dgf ) - 1 )  ) * cos( 2 * Math.PI * u2)

    (mu + sigma * dgf, rng2)
  }
}
