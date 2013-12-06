package org.lanyard.dist.disc.markov

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.dist.disc.Discrete
import org.lanyard.random.RNG
import org.lanyard.util.Util
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

abstract class Markov[A] private[markov] (val length: Int, val order: Int, val probs: Array[Array[Double]])(implicit disc: Discrete[A]) extends Distribution[List[A]] {

  import math._
  import disc._

  /** Bit mask where all bits to store an element are set. */
  private val mask: Int = (1 << (bitsPerSymbol * order)) - 1

  def offset(position: Int): Int

  /**
   * Computes the likelihood of the given sequence.
   *
   * @param value sequence of discrete elements
   * @return likelihood of the sequence
   */
  def apply(value: List[A]): Double = exp(logLike(value))

  /**
   * Computes the log likelihood of the given sequence.
   *
   * @param sequence sequence of discrete elements
   * @return log likelihood of the sequence
   */
  override def logLike(sequence: List[A]): LogLike = {
 
    @tailrec def rec(sequence: List[A], position: Int, hash: Int, like: LogLike): LogLike = {
      if (sequence.isEmpty) {
        like
      } else {
        val idx = asInt(sequence.head)
        val off = offset( position )
        val newHash = nextHash(hash, idx, position)
        rec(sequence.tail, position + 1, newHash, like + log(probs(off + hash)(idx)))
      }
    }

    rec( sequence, 0, 0, 0.0 )
  }

  /**
   * Computes the hash of the next precursors.
   *
   * @param prevHash hash of the previous precursors
   * @param position position in the sequence
   * @param next next symbol
   * @return hash of the next precursor
   */
  def nextHash(prevHash: Int, next: Int, position: Int): Int = 
    if (position <= order - 1) { // there are less than `order` symbols to the left
      prevHash | // no need to delete least significant bits
        (next << (bitsPerSymbol * position)) // add new most significant bits
    } else { // there are `order` symbols to the left
      (prevHash >>> bitsPerSymbol) | // delete bits of least significant bits
        (next << (bitsPerSymbol * (order - 1))) & mask // add new most significant bits and cut of rest
    }

  /**
   * Draws a random sequence from the markov chain.
   *
   * @param source source of randomness
   * @return random sequence of length `length`
   */
  def random(source: RNG): (List[A], RNG) = {

    import Util._

    val buffer = new ListBuffer[A]()
    buffer.sizeHint(length)

    // draw and add first element
    val (firstElement, rngTmp) = drawFrom(source, probs(0))
    buffer += fromInt(firstElement)

    // draw following element depending on the previous
    var rng = rngTmp
    var hash = firstElement
    var i = 1
    while (i < length) {
      val off = offset( i )
      val (idx, tmpRNG) = drawFrom(rng, probs(off + hash))
      val draw = fromInt(idx)

      hash = nextHash(hash, idx, i)
      buffer += draw

      rng = tmpRNG
      i += 1
    }

    (buffer.result, rng)
  }

}
