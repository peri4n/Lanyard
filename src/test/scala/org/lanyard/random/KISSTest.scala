package org.lanyard.random

import java.io.PrintWriter
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class KISSTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The KISS RNG") {

    val DrawsPerTest = 100000

    it("succeeds in the Test published by George Marsaglia") {
      val rng = new KISS
      val (draw, nextRNG) = rng.forward(99999999).nextLong
      draw should be(1666297717051644203L)
    }

    it("should draw doubles in the range of (0,1).") {
      forAll(( Gen.posNum[Long], "seed")) { (seed: Long) =>
        val rng = KISS(seed)
        val samples = Random.double.randoms(rng).take(DrawsPerTest)
        samples.foreach{ _ should be > 0.0 }
        samples.foreach{ _ should be < 1.0 }
      }
    }

    it("should produce an average of 0.5") {
      forAll(( Gen.posNum[Long], "seed")) { (seed: Long) =>
        val rng = KISS(seed)
        val samples = Random.double.randoms(rng).take(DrawsPerTest)
          (samples.sum / DrawsPerTest) should be( 0.5 +- 1E-2)
      }
    }
  }
}
