package org.lanyard.util

/** The incomplete beta function. */
case class IncompleteBeta( val a: Double, val b: Double ) extends PartialFunction[Double,Double] {

  require( a > 0, "Incomplete beta function parameter a needs to be strictly positive. Found value: " + a )
  require( b > 0, "Incomplete beta function parameter b needs to be strictly positive. Found value: " + b )

  /** Checks if the incomplete beta is defined at a given value.
    * 
    * @param value value to be checked
    * @return true if value is in [0,1], false otherwise
    */
  override def isDefinedAt( value: Double ): Boolean = 0.0 < value || value < 1.0

  /** Computes the value of the incomplete beta function at a given value.
    * 
    * @param value value to compute the incomplete beta for
    * @return incomplete beta of value
    */
  def apply( value: Double ): Double = {
    if( value == 0 || value == 1 ) {
      value
    } else if( a > 3000 && b > 3000 ) {
      approximate( value )
    } else {
    }
  }

  private def continuedFraction( value: Double ): Double = ???

  private def approximate( value: Double): Double = ???


}
