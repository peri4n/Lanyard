package org.lanyard.dist.disc

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

abstract class Binomial ( val n: Int, val p: LogLike ) extends Distribution[Int] {

  private val prob = if( p <= 0.5 ) p else 1 - p

  def apply( value: Int ): LogLike = 
    if( 0 <= value && value <= n ) {
      0.0
    } else {
      Double.NegativeInfinity
    }

  private def symmetricK( k: Int): Int = if( p == prob ) k else n - k

}

object Binomial {

  class SmallNumber(n: Int, p: Double) extends Binomial(n, p) {

    def random( source: RNG ): (Int, RNG) = {
      var (i, k) = (0, 0)
      var rng = source
      while( i < n ) {
        var (doub, nextRNG) = rng.nextDouble
        if( doub < p ) {
          k += 1
        }
        rng = nextRNG
      }

      (symmetricK( k ), rng)
    }
  }

  class SmallMean(n: Int, p: Double) extends Binomial(n, p) {

    import math._

    val cdf = new Array[Double]( 64 )
    cdf(0) = exp( n * log( 1 - prob ))
    for( j <- 1 until 64 ) {
      cdf( j ) = cdf( j - 1) + exp(
        LogGamma( n + 1.0) - LogGamma( j + 1.0) - LogGamma( n - j + 1.0) + j * log( prob ) +
          (n - j) * log( 1.0 - prob))
    }

    def random( source: RNG): (Int, RNG) = {
      val (draw1, rng) = source.nextDouble
      var kl = -1
      var k = 64
      while( k - kl > 1 ) {
        var km = (kl + k) / 2
        if( draw1 < cdf(km) ) {
          k = km
        } else {
          kl = km
        }
      }

      (symmetricK( k ), rng)
    }
  }

  class LargeMean(n: Int, p: Double) extends Binomial(n, p) {

    def random( source: RNG): (Int, RNG) = null
  }

  def apply( n: Int, p: LogLike ): Binomial = ( n, p ) match {
    case (n, p) if n < 64 => new SmallNumber(n, p)
    case (n, p) if n * p < 30.0 => new SmallMean(n, p)
    case _ => new LargeMean(n, p)
  }

}
