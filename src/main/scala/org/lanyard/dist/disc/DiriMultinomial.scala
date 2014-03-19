package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class DiriMultinomial() extends Distribution[List[Int]] {

  override def apply( values: List[Int] ): Double = 0.0

  def random( source: RNG ): (List[Int], RNG) = ???

}
