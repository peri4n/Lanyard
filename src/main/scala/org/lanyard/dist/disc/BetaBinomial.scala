package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class BetaBinomial( alpha: Double, beta: Double ) extends Distribution[Int] {

  override def apply( value: Int ): Double = 0.0

  def random( source: RNG ): (Int, RNG) = ???

}
