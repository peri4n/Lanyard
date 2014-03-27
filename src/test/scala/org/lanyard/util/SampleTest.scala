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
  }
}
