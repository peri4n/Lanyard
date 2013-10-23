package org.lanyard.dist.disc

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DiscreteTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The class for discrete types") {

    it("can be constructed by an array") {


    }

    it("can output the markov hashes") {
      sealed trait DNA
      case object A extends DNA
      case object C extends DNA
      case object G extends DNA
      case object T extends DNA
      val dna = Discrete( A, C, G, T )

      val seq = Array(A, C, G, A, T, A, T, G)
      val hashes = dna.markovHashes( seq, 0 )
      println( hashes.mkString(" ") )
    }
  }
}
