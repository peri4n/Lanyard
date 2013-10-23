package org.lanyard.dist.disc

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Uniform( leftLimit: Int, rightLimit: Int) extends Distribution[Int] {

  require( leftLimit <= rightLimit, 
    "Discrete uniform distribution parameters leftLimit and rightLimit have to form a valid interval. Found value: [" + leftLimit + ", " + rightLimit + " ]" )

  private val size = rightLimit - leftLimit + 1

  def apply( value: Int ) = 1.0 / size

  def mean: Double = ( rightLimit + leftLimit ).toDouble / 2

  def variance: Double = ( size * size - 1.0 ) / 12

  def random( source: RNG ): (Int, RNG) = {
    val (draw, nextRNG) = source.nextInt
    ( (draw % size) + leftLimit, nextRNG )
  }
}
