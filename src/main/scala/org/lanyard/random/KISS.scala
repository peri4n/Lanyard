package org.lanyard.random

/** 64-bit version of the KISS (P)RNG [[http://mathforum.org/kb/message.jspa?messageID=6627731 proposed]] by George Marsaglia himself.
  * It is composed out of three simple but fast RNGs each nearly good enough to serve alone but succeeding in cooperation.
  */
class KISS(
  val x: Long = 1234567890987654321L,
  val y: Long = 362436362436362436L,
  val z: Long = 1066149217761810L,
  val c: Long = 123456123456123456L ) extends RNG {

  /** Computes a step of the RNG.
    * 
    * @return Pair of a random `Long` and an updated KISS generator.
    */
  def nextLong: ( Long, KISS ) = {
    val ( newX, newC ) = mwc( x, c )
    val newY = xsh( y )
    val newZ = cng( z )
    ( newX + newY + newZ, new KISS( newX, newY, newZ, newC ) )
  }

  /** Extracts the sign bit of a `Long`. */
  private def signBit( x: Long ): Long = x >>> 63

  /** Computes a X-or-shift step. */
  private def xsh( y: Long ): Long = {
    var newY = y ^ ( y << 13 )
    newY ^= ( newY >>> 17 )
    newY ^= ( newY << 43 )
    newY
  }

  /** Computes a [[http://en.wikipedia.org/wiki/Linear_congruential_generator Linear congruential generator]]
    * step.
    */
  private def cng( z: Long ): Long = z * 6906969069L + 1234567L

  /** Computes a [[http://en.wikipedia.org/wiki/Multiply-with-carry Multiply-with-Carry]] step. */
  private def mwc( x: Long, c: Long ): ( Long, Long ) = {
    val t = ( x << 58 ) + c
    if ( signBit( x ) == signBit( t ) ) {
      ( x + t, ( x >>> 6 ) + signBit( x ) )
    } else {
      ( x + t, ( x >>> 6 ) + 1L - signBit( x + t ) )
    }
  }
}

object KISS {

  def apply( seed: Long ): KISS = {
    val rng = Ranq1( seed )
    val (draw1, rng1) = rng.forward(10).nextLong
    val (draw2, rng2) = rng1.forward(10).nextLong
    val (draw3, rng3) = rng2.forward(10).nextLong
    val (draw4, rng4) = rng3.forward(10).nextLong
    new KISS( draw1, draw2, draw3, draw4 )
  }

}
