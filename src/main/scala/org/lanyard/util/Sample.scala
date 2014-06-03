package org.lanyard.util

import org.lanyard.random.RNG
import scala.annotation.tailrec

/** Sample function */
object Sample {

  /**
   * Samples from a weighted set of symbols
   *
   * @param symbols symbols to sample from
   * @param weights weights of the symbols
   * @return a random symbol and the updated generator
   */
  def apply[A](symbols: List[A], weights: List[Double], source: RNG): (A, RNG) = {
    val (draw, nextRNG) = source.nextDouble

    /** Recurses as long as the accumulated weights are smaller than draw */
    @tailrec def rec(symbols: List[A], weights: List[Double], acc: Double, threshold: Double): A =
      if (weights.head + acc > threshold) 
        symbols.head
      else 
        rec(symbols.tail, weights.tail, acc + weights.head, threshold)

    (rec(symbols, weights, 0, draw), nextRNG)
  }

  def fromArray( probabilities: Array[Double], source: RNG ): (Int, RNG) = {
    var i = 0
    var (draw, nextRNG) = source.nextDouble
    while( draw > probabilities(i) && i < probabilities.length ) {
      println( i , draw )
      draw -= probabilities(i)
      i += 1
    }
    (i, nextRNG )
  }

}
