package org.lanyard.dist.disc


import org.lanyard.dist.Distribution
import org.lanyard.inference.ML



case class Binomial ( val n: Int, val p: Double ) extends Distribution[Int] with ML[Int, Binomial] {

  import math._
  import Binomial._

  private val prob = if( p < 0.5 ) p else ( 1- p )

  private val npq =  n * prob * ( 1 - prob )
  private val sqnpq = sqrt( npq )
  private val c = n * prob + 0.5
  private val b = 1.15 + 2.53 * sqnpq
  private val a = -0.0873 + 0.0248 * b + 0.01 * prob
  private val alpha = ( 2.83 + 5.1 / b ) * sqnpq
  private val r = prob / ( 1 - prob )
  private val ur = 0.43
  private val vr = 0.92 - 4.2 / b
  private val urvr = 0.86 * vr
  private val m = floor(( n + 1) * prob ).toInt
  private val nr = (r * ( n + 1 ) )

  def train( samples: List[Int] ) : Binomial = ???

  import org.lanyard._

  def apply( value: Int ): LogLike = 
    if( 0 <= value && value <= n ) {
      0.0
    } else {
      Double.NegativeInfinity
    }

  import org.lanyard.random.RNG

  def random( source: RNG): (Int, RNG) = {
    
    /** In this case variates are drawn using the BINV algorithm.
      * It is a fall back for small means and is only used because
      * the BTRD algorithm is no sufficient approximation in this case. 
      */
    def invert: (Int, RNG) = {
      val q = 1 - p
      val s = p / q
      val a = ( n + 1 ) * s
      val (u, nextRNG) = source.nextDouble
      var tmpU = u
      var r = pow( q, n)
      var x = 0
      while( tmpU > r ) {
        tmpU -= r
        x += 1
        r *= (( a / x) - s )
      }
      ( x, nextRNG)
    }

    import scala.annotation.tailrec

    /** This is the BTRD algorithm */
    @tailrec def btrd( rng: RNG): (Int, RNG) = {

      def correction( k: Int ): Double =
        if( k < 10 ) {
          table(k)
        } else {
          val kPlus1Sq = 1.0 / ( k + 1 )
          ( 1.0 / 12 - ( 1.0 /360 - 1.0/1260 * kPlus1Sq * kPlus1Sq ) * kPlus1Sq * kPlus1Sq ) * kPlus1Sq
        }

      def symmetricK( k: Int ): Int = if( p == prob ) k else n - k

      val (doub, nextRNG) = rng.nextDouble
      var v = doub
      var returnRNG = nextRNG
      if( v <= urvr ) {
        /** STEP 1 */
        val u = v / vr - 0.43
        (symmetricK( floor((2 * a / ( 0.5 - abs(u) ) + b ) * u + c ).toInt ), returnRNG)
      } else {
        /** STEP 2 */
        var u = 0.0
        if( v >= vr ) { 
          /** Step 2.1 */
          val (doub, nextRNG1) = nextRNG.nextDouble
          returnRNG = nextRNG
          u = doub - 0.5
        } else { 
          /** STEP 2.2 */
          u = v / vr - 0.93
          u = signum(u) * 0.5 - u
          val (doub, nextRNG1) = nextRNG.nextDouble
          returnRNG = nextRNG
          v = doub * vr
        }

        /** STEP 3 */
        val us = 0.5 - abs(u)
        val k = floor(( 2 * a / us + b) * u + c).toInt
        if ( k < 0 || k > n ) {
          btrd( returnRNG )
        } else {
          v *= alpha / ( a / ( us * us ) + b)
          val km = abs( k - m )
          if( km <= 15 ) {
            /** STEP 3.1 
              * Recursive evaluation of f(k) */
            var f = 1.0
            if( m < k ) {
              var i = m
              do {
                i += 1
                f *= (nr / i - r) 
              } while( i != k )
            } else if ( m > k ) {
              var i = k
              do {
                i += 1
                v *= ( nr / i - r )
              } while( i != m )
            } 
            
            if( v <= f ) {
              (symmetricK( k ), returnRNG)
            } else {
              btrd( returnRNG)
            }
          } else { 
            /** STEP 3.2 
              * Squeeze-acceptance or rejection */
            v = log(v)
            val rho = ( km / npq ) * ((( km / 3 + 0.625 ) * km + 1.0/6) / npq + 0.5)
            val t = -km * km / (2 * npq)
            if( v < t - rho ) {
              println( "STEP3.2" )
              ( symmetricK( k ) , returnRNG )
            } else if( v > t + rho ) {
              btrd( returnRNG )
            } else {
              /** STEP 3.3
                * Set-up for step 3.4 */
              val nm = n - m + 1
              val h = (m + 0.5) * log((m + 1) / ( r * nm)) + correction(m) + correction(n - m)
              
              /** STEP 3.4
                * Final acceptance-rejection test */
              val nk = n - k + 1
              if( v <= h + (n + 1) * log(nm.toDouble / nk ) + ( k + 0.5 ) * log(nk * r / (k + 1)) - correction(k) - correction(n - k) ) {
                println( "STEP3.4" )
                (symmetricK( k ), returnRNG)
              } else {
                btrd(returnRNG)
              }
            }
          }
        }
      }
    }

    if ( n * p < 11 )
      invert
    else
      btrd( source )
  }
}

object Binomial {

  private val table = Array(
    0.08106146679532726, 
    0.04134069595540929, 
    0.02767792568499834,
    0.02079067210376509,
    0.01664469118982119,
    0.01387612882307075,
    0.01189670994589177,
    0.01041126526197209,
    0.009255462182712733,
    0.008330563433362871)

}
