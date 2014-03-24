package org.lanyard.util

/** The incomplete beta function. */
case class IncBeta( val a: Double, val b: Double ) extends PartialFunction[Double,Double] {

  require( a > 0, "Incomplete beta function parameter a needs to be strictly positive. Found value: " + a )
  require( b > 0, "Incomplete beta function parameter b needs to be strictly positive. Found value: " + b )

  /** precision of computation*/
  private val EPS = 1E-9

  /** tiny value to shift */
  private val TINY = 1E-30

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
      continuedFraction( value )
    }
  }

  /** Computes a value of the incomplete beta function.
    * 
    * I uses the method proposed in: ''W.J. Lentz, 1976. Generating Bessel functions in Mie scattering 
    * calculations using continued fractions, Applied Optics, vol 15, 668''
    * to compute the continues fraction defined in:
    * ''Milton Abramowitz, and Irene A. Stegun. 1964. Handbook of Mathematical Functions with Formulas, Graphs, 
    * and Mathematical Tables. 9th Edition. Dover, New York''.
    * 
    * @param value value to compute the incomplete beta for
    * @return incomplete beta of value
    */    
  private def continuedFraction( value: Double ): Double = {

    import math._

    /** Computes the even step in the Lentz method */
    @inline def evenStep( iter: Int): Double = ( b - iter ) * value / ( (a - 1 + 2 * iter) * ( a + 2 * iter) )

    /** Computes the odd step in the Lentz method */
    @inline def oddStep( iter: Int): Double = - ( a + iter ) * (a + b + iter) * value / ( ( a + 2 * iter) * ( a + 1 + 2 * iter) )

    /** Shifts a value if it is small enough */
    @inline def tinyShift( value: Double): Double = if( abs( value ) < TINY ) TINY else value

    var c = 1.0
    var d = tinyShift( 1.0 - (a + b) * value / ( a + 1.0) )
    d = 1.0 / d

    var coef = 0.0
    var result = d
    var unsufficientPrecision = true

    var i = 1
    while( i < 10000 && unsufficientPrecision ) {
      coef = evenStep( i ) // even step
      d = tinyShift( 1.0 + coef * d )
      c = tinyShift( 1.0 + coef / c )
      d = 1.0 / d
      result *= d * c

      coef = oddStep( i ) // odd step
      d = tinyShift( 1.0 + coef * d )
      c = tinyShift( 1.0 + coef / c )
      d = 1.0 / d
      result *= d * c
      if( abs( d * c - 1.0 ) <= EPS ) {
        unsufficientPrecision = false
      }
    }
    result
  }

  private def approximate( value: Double): Double = ???


}
