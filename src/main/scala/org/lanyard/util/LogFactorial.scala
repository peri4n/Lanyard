package org.lanyard.util

/** Function representing the logarithm of the factorial. */
object LogFactorial extends Function[Int, Double] {

  private val Max = 2000

  private val logFactorials = {
    val tmp = new Array[Double]( Max )
    for( i <- 0 until Max ) {
      tmp(i) = LogGamma( i + 1)
    }
    tmp
  }

  def apply( value: Int): Double = {
    require( 0 <= value, "Argument of LogFactorial needs to be in range [0, Inf). Found value: " + value )
    if( 0 < value && value < Max ) logFactorials( value ) else  LogGamma( value + 1 )
  }

}
