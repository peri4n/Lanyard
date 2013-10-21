package org.lanyard.dist.cont

import org.lanyard.LogLike
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import scala.annotation.tailrec

/** Gamma distribution.
  * 
  */
class GammaDist private( val shape: Double, val scale: Double, private val oshape: Double ) extends Distribution[Double] {

  import GammaDist._

  require( oshape > 0.0, "Gamma distribution parameter shape needs to be strictly positive. Found value: " + shape )

  def rate = 1 / scale

  def mean = oshape * scale

  def variance = oshape * scale * scale

  def skewness = 2 / math.sqrt( oshape )

  def kutorsis = 6 / oshape

  def apply( value: Double): LogLike = 0.0

  @tailrec
  final def random( source: RNG ): (Double, RNG) = {
    val a1 = shape - 1.0 / 3.0
    val a2 = 1.0 / math.sqrt(9.0 * a1)

    @tailrec
    def genNormal( s: RNG) : (Double, Double, RNG) = {
      val (x, rng) = standardNormal.random(s)
      val v = 1.0 + a2 * x
      if( v <= 0.0 ) genNormal(rng) else (x, v, rng)
    }

    var (x, v, rng1) = genNormal(source)
    v = v * v * v
    val (u, rng2) = rng1.nextDouble
    if( u > 1.0 - 0.331 * x * x * x * x && math.log(u) > 0.5 * x * x + a1 * (1.0 - v + math.log(v)))
      random(rng2)
    else {
      if( shape == oshape ) {
        (a1 * v * scale, rng2)
      } else {
        val (u, rng3) = rng2.nextDouble
        (math.pow(u, 1.0 / oshape) * a1 * v * scale, rng3)
      }
    }
  }
}

object GammaDist {

  private val standardNormal = Gaussian()

  def apply( shape: Double, scale: Double ): GammaDist = if( shape < 1.0 ) new GammaDist( shape + 1, scale, shape) else new GammaDist(shape, scale, shape)


}
