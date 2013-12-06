package org.lanyard.dist.disc.markov

import org.lanyard.dist.disc.Discrete
import org.lanyard.random.KISS
import org.scalatest.FunSpec
import org.scalatest.Matchers

class InMarkovTest extends FunSpec with Matchers {

  describe("Inhomogeneous markov models") {

    it("computes the likelihood of a sequence") {

      /** define DNA */
      sealed trait DNA
      case object A extends DNA
      case object C extends DNA
      case object G extends DNA
      case object T extends DNA
      implicit val dna = Discrete( A, C, G, T )
      val seq = List(A, C, G, A )

      val probs = Array(
        Array(1.0, 0.0, 0.0, 0.0),
        Array(0.0, 1.0, 0.0, 0.0),
        Array(0.0, 0.0, 1.0, 0.0),
        Array(1.0, 0.0, 0.0, 0.0),
        Array(0.0, 1.0, 0.0, 0.0))

      val markov = InMarkov( 5, 0, probs )
      markov.logLike( List(A, C, G, A, C) ) should be (0.0)

    }
  }
}
