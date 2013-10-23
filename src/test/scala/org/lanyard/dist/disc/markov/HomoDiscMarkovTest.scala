package org.lanyard.dist.disc.markov

import org.lanyard.dist.disc.Discrete
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class HomoDiscMarkovTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("Homogeneous discrete markov chain") {

    it("computes the likelihood of a sequence") {

      sealed trait DNA
      case object A extends DNA
      case object C extends DNA
      case object G extends DNA
      case object T extends DNA
      implicit val dna = Discrete( A, C, G, T )
      val seq = Array(A, C, G, A )

      val markov1 = new HomoDiscMarkov(0, Array(Array(0.25, 0.25, 0.25, 0.25)) )

      val like = markov1.logLike( seq )
      println( like )

      val probs = Array(
        Array(1.0, 0.0, 0.0, 0.0), // start probs
        Array(0.0, 1.0, 0.0, 0.0), // given A
        Array(0.0, 0.0, 1.0, 0.0), // given C
        Array(1.0, 0.0, 0.0, 0.0), // given G
        Array(0.0, 0.0, 0.0, 0.0)) // given T


      val markov2 = new HomoDiscMarkov(1, probs )

      val like2 = markov2( seq )
      println( like2 )

    }
  }
}
