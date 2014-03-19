package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Multinomial( probs: Seq[Double] ) extends Distribution[List[Int]] {

  import math._

  require( abs(probs.sum - 1.0) <= 1E-10, s"Multinomial distribution parameter probs has to sum to one. Found sum: ${probs.sum}")

  override def apply( values: List[Int] ): Double = 0.0

  def random( source: RNG ): (List[Int], RNG) = ???

}
