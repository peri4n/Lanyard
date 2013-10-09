package org.lanyard.dist.cont

import org.lanyard.Prob
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import scala.annotation.tailrec

class GammaDist( val shape: Double, val scale: Double ) extends Distribution[Double] {

  require( shape > 0.0, "Gamma distribution parameter shape needs to be strictly positive. Found value: " + shape )

  private val a1 = shape - 1.0 / 3.0
  private val a2 = 1.0 / math.sqrt(9.0 * a1)

  private val standardNormal = NormalDist()

  val rate = 1 / scale

  val mean = shape * scale

  val variance = shape * scale * scale

  val skewness = 2 / math.sqrt( shape )

  val kutorsis = 6 / shape

  def apply( value: Double): Prob = 0.0

  @tailrec
  final def random( source: RNG): (Double, RNG) = {

    @tailrec
    def genNormal( s: RNG) : (Double, Double, RNG) = {
      val (x, rng) = standardNormal.random(s)
      val v = 1.0 + a2 * x
      if( v <= 0.0 ) (x, v, rng) else genNormal(rng)
    }

    var (x, v, rng1) = genNormal(source)
    v = v * v * v
    val (u, rng2) = rng1.nextDouble
    if( u > 1.0 - 0.331 * x * x * x * x && math.log(u) > 0.5 * x * x + a1 * (1.0 - v + math.log(v)))
      if( shape >= 2.0 ) {
        (a1 * v * scale, rng2)
      } else {
        val (d, rng3) = rng2.nextDouble
        (math.pow(d, 1.0 / ( shape - 1)) * a1 * v * scale, rng3)
      }     
    else 
      random(rng2)
  }
}

object GammaDist {

  def apply( shape: Double, scale: Double ): GammaDist = if( shape < 1.0 ) new GammaDist( shape + 1, scale) else new GammaDist(shape, scale)

}
