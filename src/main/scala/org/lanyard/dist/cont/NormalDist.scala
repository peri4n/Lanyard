package org.lanyard.dist.cont

import org.lanyard.Prob
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import scala.annotation.tailrec

case class NormalDist( mean: Double = 0, variance: Double = 1) extends Distribution[Double] {

  require( variance > 0, "Normal distribution parameter variance needs to be strictly positive. Found value: " + variance )

  def stdDeviation = math.sqrt( variance )

  def skewness = 0.0

  def kurtosis = 0.0

  def apply( value: Double): Prob = 0.0

  @tailrec
  final def random( source: RNG): (Double, RNG) = {
    val (draw1, rng1) = source.nextDouble
    val (draw2, rng2) = rng1.nextDouble
       val v = 1.7156 * (draw2 - 0.5)
    val x = draw1 - 0.449871
    val y = math.abs(v) + 0.386595
    val q = (x * x) + y * ( 0.19600 * y - 0.25472 * x)
    if( q > 0.27597 && ( q > 0.27846 || (v*v) > -4.0 * math.log(draw1) * draw1 * draw1 ))
      random(rng2)
    else
      (mean + stdDeviation * v / draw1, rng2)
  }
}
