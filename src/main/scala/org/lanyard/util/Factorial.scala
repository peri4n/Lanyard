package org.lanyard.util

/** The factorial function. */
object Factorial extends Function[Int,Double] {

  /** Biggest `Int` for which the factorial can be stored in a `Double` */
  val MaxInput = 170

  /** Precompute all factorials */
  private val factorials = {
    val tmp = new Array[Double]( MaxInput + 1 )
    tmp( 0 ) = 1.0
    for( i <- 1 to MaxInput) {
      tmp( i ) = tmp( i - 1 ) * i
    }
    tmp
  }

  /** Computes the factorial function.
    * 
    * @param value value to compute the factorial for
    * @return factorial of value
    */
  def apply( value: Int ): Double = {
    require( 0 <= value && value <= MaxInput, 
      s"Argument of Factorial is out of range. It needs to be in [0, ${MaxInput}]. Found value = " + value )
    factorials( value )
  }
}
