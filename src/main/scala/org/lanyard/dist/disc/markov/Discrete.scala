package org.lanyard.dist.disc.markov

trait Discrete[A] {

  def asInt( x: A ): Int

  def fromInt( x: Int): A

}









