package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import scala.annotation.switch

case class Bernoulli( p: Double ) extends Distribution[Boolean] {

  def apply( value: Boolean ): Double = (value: @switch) match {
    case true => p
    case _ => ( 1 - p )
  }

  def random( source: RNG ): (Boolean, RNG) = {
    val (doub, nextRNG) = source.nextDouble
    (doub > p, nextRNG)
  }

}
