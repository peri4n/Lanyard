package org.lanyard.util

/** The factorial function. */
object Factorial extends PartialFunction[Int,Double] {

  /** Precompute all factorials */
  val values = {
    val tmp = new Array[Double]( 171 )
    tmp( 0 ) = 1.0
    var i = 1
    while ( i < 171 ) {
      tmp( i ) = tmp( i - 1 ) * i
      i += 1
    }
    tmp
  }

  /** Checks if the factorial is defined at a given value.
    * 
    * @param value value to be checked
    * @return true if value is in [0, 170], false otherwise
    */
  override def isDefinedAt( value: Int ): Boolean = 0 <= value && value <= 170

  /** Computes the factorial function a given value.
    * 
    * @param value value to compute the factorial for
    * @return factorial of value
    */
  def apply( value: Int ): Double = {
    require( isDefinedAt(value) , "Argument of Factorial is out of range. It needs to be in [0, 170]. Found value = " + value )
    values( value )
  }
}
