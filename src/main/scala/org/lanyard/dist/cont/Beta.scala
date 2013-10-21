package org.lanyard.dist.cont

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

case class Beta private( val alpha: Double, val beta: Double) extends Distribution[Double] {

  require( alpha > 0, "Beta distribution parameter alpha needs to be stricly positive. Found value: " + alpha)
  require( beta > 0, "Beta distribution parameter beta needs to be stricly positive. Found value: " + beta)

  private val alphaGamma = GammaDist(alpha, 1)
  private val betaGamma = GammaDist(beta, 1)

  /** Precomputes the constant term used in the probability density function. */
  private val constantTerm = LogGamma( alpha + beta ) - LogGamma( alpha ) - LogGamma( beta )

  val mean = alpha / (alpha + beta)

  val variance = ( alpha * beta ) / ((alpha + beta) * (alpha + beta) * ( alpha + beta + 1))

  val mode = (alpha - 1) / ( alpha + beta - 2)

  /** Computes the logarithm of the probability density function.
    * 
    * @param value value to compute the log probability for
    * @return logarithim of the probability if value is in [0,1], negative infinity otherwise
    */
  def apply( value: Double): LogLike = 
    if( 0 <= value && value <= 1)
      constantTerm + ( alpha - 1.0 ) * math.log( value ) + ( beta - 1.0 ) * math.log( 1.0 - value )
    else Double.NegativeInfinity
  
  /** Draws a random sample from this beta distribution.
    * 
    * @param source source of randomness
    * @return pair of a beta sample and the updated RNG
    */
  def random( source: RNG): (Double, RNG) = {
    val (d1, rng1) = alphaGamma.random(source)
    val (d2, rng2) = betaGamma.random(rng1)
    (d1 / (d1 + d2), rng2)
  }
}

