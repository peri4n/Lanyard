package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Poisson ( lambda: Double ) extends Distribution[Int] {

  import math._

  override def apply( value: Int ): Double = if( value < 0 ) 0 else 0

  def random( source: RNG ): (Int, RNG) = ???
}
