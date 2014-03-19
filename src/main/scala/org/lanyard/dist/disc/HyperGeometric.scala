package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/**
  * 
  * @constructor Creates a Hypergeometric distribution
  * @param size population size (e.g. number of balls)
  * @param successes number of successes in the population (e.g. number of black balls) 
  * @param draws number of draws
  */
case class HyperGeometric( size: Int, successes: Int, draws: Int  ) extends Distribution[Int] {

  require( 0 <= size, s"Hypergeometric distribution parameter size needs to be non-negative. Found value: ${size}")
  require( 0 <= successes && successes <= size, s"Hypergeometric distribution parameter successes needs to be in range [0, ${size}], Found value: ${successes}")
  require( 0 <= draws && draws <= size, s"Hypergeometric distribution parameter draws needs to be in range [0, ${size}], Found value: ${draws}")

  override def apply( value: Int ): Double = 0.0

  def random( source: RNG ): (Int, RNG) = ???

}
