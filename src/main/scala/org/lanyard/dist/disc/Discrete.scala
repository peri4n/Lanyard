package org.lanyard.dist.disc

import scala.collection.immutable.HashMap

/** Typeclass for a discrete alphabet. It serves as a translation from elements of type `A` to `Int`.
  * 
  * @tparam A type of elements
  */
trait Discrete[A] {

  import math._

  /** Translates `elem` to an `Int`.
    * @param elem element of the alphabet
    * @return rank of the element
    */
  def asInt( elem: A ): Int

  def fromInt( rank: Int): A

  def size: Int

  def bitsPerSymbol: Int = ceil( log( size.toDouble ) / log( 2.0 ) ).toInt

  def markovHashes( seq: IndexedSeq[A], order: Int ): Array[Int] = {
    val hashes = new Array[Int]( seq.length )
    hashes( 0 ) = asInt( seq( 0 ) )

    var pos = 1
    while( pos < seq.length ) {
      val element = asInt( seq(pos) )
      if( pos <= order ) {
        hashes( pos ) = hashes( pos - 1 ) + (element * pow( size, pos )).toInt
      } else {
        hashes( pos ) = (hashes( pos - 1 ) >>> bitsPerSymbol) + (element * pow( size, order )).toInt
      }
      pos += 1
    }
    hashes
  }

}

object Discrete {

  def apply[A]( elements: A* ): Discrete[A] = new Discrete[A] {
    
    private val ranks = elements.toIndexedSeq

    private val dict = (HashMap.empty[A, Int] ++ elements.zipWithIndex)(_)

    def fromInt( rank: Int ): A = ranks(rank)

    def asInt( elem: A): Int = dict(elem)

    val size = ranks.length

  }

  def fromSeq[A]( elements: Seq[A] ): Discrete[A] = apply( elements: _* )

}









