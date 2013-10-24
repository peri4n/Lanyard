package org.lanyard.dist.cont

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

/** The beta distribution is a continuous probability distribution.
  * 
  * @constructor Creates a beta distribution with given parameters.
  * 
  * @param alpha alpha parameter of the beta distribution
  * @param beta beta parameter of the beta distribution
  */
case class Beta( val alpha: Double, val beta: Double) extends Distribution[Double] {

  import math._

  require( alpha > 0, "Beta distribution parameter alpha needs to be stricly positive. Found value: " + alpha)
  require( beta > 0, "Beta distribution parameter beta needs to be stricly positive. Found value: " + beta)

  /** Gamma distributions used for sampling */
  private val alphaGamma = Gamma(alpha, 1)
  private val betaGamma = Gamma(beta, 1)

  /** Precomputes the constant term used in the probability density function. */
  private lazy val constantTerm = LogGamma( alpha + beta ) - LogGamma( alpha ) - LogGamma( beta )

  /** Computes the mean. */
  def mean = alpha / (alpha + beta)

  /** Computes the variance. */
  def variance = ( alpha * beta ) / ((alpha + beta) * (alpha + beta) * ( alpha + beta + 1))

  /** Computes the mode. */
  def mode = (alpha - 1) / ( alpha + beta - 2)

  /** Computes the probability density function.
    * 
    * @param value value in [0,1]
    * @return value of the density
    */
  def apply( value: Double ): Double = exp( logLike(value) )

  /** Computes the logarithm of the probability density function.
    * 
    * @param value value to compute the log probability for
    * @return logarithim of the probability if value is in [0,1], negative infinity otherwise
    */
  override def logLike( value: Double): LogLike = 
    if( 0 <= value && value <= 1)
      constantTerm + ( alpha - 1.0 ) * log( value ) + ( beta - 1.0 ) * log( 1.0 - value )
    else 
      Double.NegativeInfinity
  
  /** Draws a random sample from this beta distribution.
    * 
    * @param source source of randomness
    * @return pair of a beta sample and the updated RNG
    */
  def random( source: RNG): (Double, RNG) = {
    val (d1, rng1) = alphaGamma.random(source)
    val (d2, rng2) = betaGamma.random(rng1)
    val draw = d1 / (d1 + d2)
    assume( 0 <= draw && draw <= 1, "Draw of the beta distribution was not in [0,1]. Parameters alpha: " + alpha + " beta: " + beta )
    (draw, rng2)
  }
}

