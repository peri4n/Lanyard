package org.lanyard.dist.cont

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

case class Uniform( val leftLimit: Double, val rightLimit: Double) extends Distribution[Double] {

  require( leftLimit < rightLimit, 
    "Continuous uniform distribution parameters leftLimit and rightLimit have to form a valid interval. Found value: [" + leftLimit + ", " + rightLimit + " ]" )

  def apply( value: Double ): Double = 1.0 / ( rightLimit - leftLimit )

  def random( source: RNG ): (Double, RNG) = source.nextDouble

}
