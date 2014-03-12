package org.lanyard.util

/** Binomial coefficient. */
object BinCoef extends Function2[Int, Int, Double] {

  import math._

  /** Computes the binomial coefficient. */
  def apply(n: Int, k: Int): Double = {
    require( 0 <= n, s"BinCoef parameter n needs non-negative. Found value: n = ${n}" )
    require(0 <= k, s"BinCoef parameter k needs non-negative. Found value: k = ${k}" )
    require( k <= n, s"BinCoef parameter k needs to be smaller or equal than parameter n. Found value: n = ${n}, k = ${k}")

    if( n <= Factorial.MaxInput ) {
      floor( 0.5 + Factorial(n ) / (Factorial(k) * Factorial(n-k)))
    } else {
      floor( 0.5 + exp( LogFactorial(n) - LogFactorial(k) - LogFactorial(n-k)))
    }
  }

}
