package org.lanyard.dist.cont

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class ExponentialDist( lambda: Double ) extends Distribution[Double] {

  require( lambda > 0, "Exponential distribution parameter lambda needs to be strictly positive. Found value: " + lambda )

  type Parameter = Double

  val mean = 1 / lambda

  val variance = 1 / (lambda * lambda)

  private val logLambda = math.log(lambda)

  def apply( value: Double ): Prob = 
    if( value >= 0 ) 
      logLambda + ( - logLambda * value )
    else 
      Double.NegativeInfinity

  def random(source: RNG): (Double, RNG) = {
    val (d, rng) = source.nextDouble
    (-math.log(d) / lambda, rng)
  }
}

object ExponentialDist extends ModelFac[ExponentialDist] {

  def create( param: Double) = new ExponentialDist(param)

}
