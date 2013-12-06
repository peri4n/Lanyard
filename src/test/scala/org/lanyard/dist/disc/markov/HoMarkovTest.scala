package org.lanyard.dist.disc.markov

import org.lanyard.dist.disc.Discrete
import org.lanyard.random.KISS
import org.lanyard.random.RNG
import org.lanyard.util.Util
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class HoMarkovTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("A homogeneous markov models") {

    it("computes the likelihood of a sequence") {

      /** define DNA */
      sealed trait DNA
      case object A extends DNA
      case object C extends DNA
      case object G extends DNA
      case object T extends DNA
      implicit val dna = Discrete( A, C, G, T )
      val seq = List(A, C, G, A )

      /** markov model order 0 */
      val markov0 = new HoMarkov(4, 0, Array(Array(0.25, 0.25, 0.25, 0.25)) )

      markov0.logLike( seq ) should be( math.log( 1.0 / 256 ) )

      /** markov model order 1 */
      val probs = Array(
        Array(1.0, 0.0, 0.0, 0.0), // start probs
        Array(0.0, 1.0, 0.0, 0.0), // given A
        Array(0.0, 0.0, 1.0, 0.0), // given C
        Array(1.0, 0.0, 0.0, 0.0), // given G
        Array(0.0, 0.0, 0.0, 0.0)) // given T
      val markov1 = new HoMarkov(4, 1, probs )
      markov1.logLike( seq ) should be( 0.0 )
  
    }
  }
}
