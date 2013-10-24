package org.lanyard.dist.disc

import scala.collection.immutable.HashMap

/** Typeclass for a discrete alphabet. It serves as a translation from elements of type `A` to `Int`.
  * 
  * @tparam A type of elements
  */
trait Discrete[A] {

  import math._

  /** Convertes values of type `A` to `Int`.
    * 
    * @param elem element of the alphabet
    * @return rank of the element
    */
  def asInt( elem: A ): Int

  /** Convertes integers to values of type `A`.
    * 
    * @param rank rank of the element in the alphabet
    * @return element of the alphabet
    */
  def fromInt( rank: Int): A

  /** Returns the size of the alphabet. */
  def size: Int

  /** Computes the number of bits to store elemets of this alphabet. */
  def bitsPerSymbol: Int = ceil( log( size.toDouble ) / log( 2.0 ) ).toInt

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









