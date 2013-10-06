package org.lanyard.util

object Factorial extends ( Int => Double ) {

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

  /** Returns the factorial
    * 
    * @param n Integer value 
    * @return factorial of n
    */
  def apply( n: Int ): Double = {
    assert( 0 <= n && n <= 170, "Factorial out of range. Argument needs to be in [0, 170]. Found value: n = " + n )
    values( n )
  }
}
