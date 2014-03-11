package org.lanyard.util

/** The factorial function. */
object Factorial extends Function[Int,Double] {

  /** Precompute all factorials */
  private val factorials = {
    val tmp = new Array[Double]( 171 )
    tmp( 0 ) = 1.0
    for( i <- 1 until 171) {
      tmp( i ) = tmp( i - 1 ) * i
    }
    tmp
  }

  /** Computes the factorial function a given value.
    * 
    * @param value value to compute the factorial for
    * @return factorial of value
    */
  def apply( value: Int ): Double = {
    require( 0 <= value && value <= 170, "Argument of Factorial is out of range. It needs to be in [0, 170]. Found value = " + value )
    factorials( value )
  }
}
