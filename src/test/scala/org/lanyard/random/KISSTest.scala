package org.lanyard.random

import java.io.PrintWriter
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class KISSTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The KISS RNG") {

    val DrawsPerTest = 100000

    it("succeeds in the Test published by George Marsaglia") {
      val rng = new KISS
      val (draw, nextRNG) = rng.forward(99999999).nextLong
      draw should be(1666297717051644203L)
    }

    it("should draw double in the range of (0,1).") {
      forAll(( Gen.choose(Long.MinValue, Long.MaxValue), "seed")) { (seed: Long) =>
        val rng = KISS(seed)
        val samples = RNG.toRandom[Double].randoms(rng).take(DrawsPerTest)
        samples.forall( s => 0 < s && s < 1) should be(true)
      }
    }

    it("should produce an average of 0.5") {
      forAll(( Gen.choose(Long.MinValue, Long.MaxValue), "seed")) { (seed: Long) =>
        val rng = KISS(seed)
        val samples = RNG.toRandom[Double].randoms(rng).take(DrawsPerTest)
          (samples.sum / DrawsPerTest) should be( 0.5 plusOrMinus 0.005)
      }
    }
  }
}
