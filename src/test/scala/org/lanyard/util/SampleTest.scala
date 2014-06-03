package org.lanyard.util

import org.lanyard.random.KISS
import org.scalatest.FunSpec
import org.scalatest.Matchers

class SampleTest extends FunSpec with Matchers {

  describe("The sample function") {

    it("returns the correct values if a weights is set to one.") {

      val rng = KISS(42)
      val ls = List(1,3,5,6)
      val weights1 = List(1.0, 0.0, 0.0, 0.0)
      val (draw1, nextRNG1) = Sample( ls, weights1, rng )
      draw1 should be (1)

      val weights2 = List(0.0, 1.0, 0.0, 0.0)
      val (draw2, netRNG2) = Sample( ls, weights2, rng )
      draw2 should be (3)

      val weights3 = List(0.0, 0.0, 1.0, 0.0)
      val (draw3, netRNG3) = Sample( ls, weights3, rng )
      draw3 should be (5)

      val weights4 = List(0.0, 0.0, 0.0, 1.0)
      val (draw4, netRNG4) = Sample( ls, weights4, rng )
      draw4 should be (6)

    }

    it("can sample from an array") {
      val rng = KISS( 42 ) // first draw is ~0.93
        println( "case1")
      val probabilities1 = Array(1.0, 0.0, 0.0)
      val (draw1, nextRNG1) = Sample.fromArray( probabilities1, rng )
      draw1 should be( 0 )

        println( "case2")
      val probabilities2 = Array(0.9, 0.05)
      val (draw2, nextRNG2) = Sample.fromArray( probabilities2, rng )
      draw2 should be( 1 )

        println( "case3")
      val probabilities3 = Array(0.45, 0.05, 0.5)
      val (draw3, nextRNG3) = Sample.fromArray( probabilities3, nextRNG1 )
      draw3 should be( 2 )
    }
  }
}
