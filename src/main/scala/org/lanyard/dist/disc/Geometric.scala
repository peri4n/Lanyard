package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Geometric( prob: Double ) extends Distribution[Int] {

  override def apply( value: Int ): Double = 0.0

  def mean: Double = 1 / prob

  def variance: Double = ( 1 - prob ) / (prob * prob)

  def random( source: RNG ): (Int, RNG) = ???

}
