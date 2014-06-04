package org.lanyard.util

/** The incomplete gamma function. */
object IncGamma {

  import math._
  import IncGamma._

  /** Assumed machine accuracy. */
  val EPS = 2.22045E-16

  val FPMIN = Double.MinPositiveValue / EPS

  /** Return the (lower) incomplete gamma function to a given value x. */
  def apply(a: Double, value: Double): Double = {
    require( value >= 0.0, s"Incomplete Gamma function argument 'value' needs to be non-negative. Found value: $value" )
    require( a > 0.0, s"Incomplete Gamma function argument 'a' needs to be strictly positive. Found value: $a")

    if( value == 0.0 )
      0.0
    else if( a.toInt >= 100 )
      approx(a, value)
    else if( value < a + 1.0 )
      ser(a, value)
    else
      1.0 - cf(a, value)

  }

  def oneMinus(a: Double, value: Double): Double = 1 - apply(a, value)

  /**
   * Returns the (upper) incomplete gamma function to a given value.
   *
   * Evaluation is done by its series representation.
   */
  private def ser(a: Double, value: Double): Double = {
    val logGamma = LogGamma(a)
    var ap = a
    var del = 1.0 / a
    var sum = del
    do {
      ap += 1
      del *= value / ap
      sum += del
    } while (abs(del) >= abs(sum) * EPS)

    sum * exp(-value + a * log(value) - logGamma)
  }

  /**
   * Returns the incomplete gamma function.
   *
   * Evaluation is done by its continuous fraction.
   */
  private def cf(a: Double, value: Double): Double = {
    val logGamma = LogGamma(a)
    var an, del = 0.0
    var b = value + 1.0 - a
    var c = 1.0 / FPMIN
    var d = 1.0 / b
    var h = d
    var i = 1
    do {
      an = -i * (i - a)
      b += 2.0
      d = an * d + b
      if (abs(d) < FPMIN) d = FPMIN
      c = b + an / c
      if (abs(c) < FPMIN) c = FPMIN
      d = 1.0 / d
      del = d * c
      h *= del
      i += 1
    } while (abs(del - 1.0) > EPS)
    exp(-value + a * log(value) - logGamma) * h
  }

  /**
   * Returns the incomplete gamma function.
   *
   * Evaluation is done by quadrature.
   */
  private def approx(a: Double, value: Double): Double = {
    val logGamma = LogGamma(a)
    val a1 = a - 1.0
    val loga1 = log(a1)
    val sqrta1 = sqrt(a1)

    val xu = if( value > a1 ) {
      max( a1 + 11.5 * sqrta1, value + 6.0 * sqrta1)
    } else {
      max( 0.0, min( a1 - 7.5 * sqrta1, value - 5.0*sqrta1))
    }

    var sum = 0.0
    var j = 0
    while( j < GaussLegendre.ngau ) {
      val t = value + (xu - value) * GaussLegendre.y(j)
      sum += GaussLegendre.w(j) * exp( - ( t - a1) + a1 *(log(t) - loga1))
      j += 1
    }
    val ans = sum * (xu - value) * exp(a1*(loga1-1.0)-logGamma)
    if( ans > 0.0 ) 1.0 - ans else - ans
  }

}










