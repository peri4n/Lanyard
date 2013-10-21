package org.lanyard.dist.disc.markov

import scala.collection.immutable.HashMap

/** Typeclass for a discrete alphabet. It serves as a translation from elements of type `A` to `Int`.
  * 
  * @tparam A type of elements
  */
trait Discrete[A] {

  /** Translates `elem` to an `Int`.
    * @param elem element of the alphabet
    * @return rank of the element
    */
  def asInt( elem: A ): Int

  def fromInt( rank: Int): A

  def size: Int

}

object Discrete {

  def apply[A]( elements: Seq[A] ): Discrete[A] = new Discrete[A] {
    
    private val ranks = elements.toIndexedSeq

    private val dict = (HashMap.empty[A, Int] ++ elements.zipWithIndex)(_)

    def fromInt( rank: Int ): A = ranks(rank)

    def asInt( elem: A): Int = dict(elem)

    val size = ranks.length

  }

}









