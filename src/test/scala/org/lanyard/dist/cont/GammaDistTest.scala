package org.lanyard.dist.cont

import java.io.PrintWriter
import org.lanyard.random.KISS
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The gamma distribution") {

    val DrawsPerTest = 100000

    it("can be sampled.") {
      forAll( (Gen.choose(Long.MinValue, Long.MaxValue), "seed")) { ( seed: Long ) =>
        val rng = KISS(seed)
        val dist = new GammaDist(2, 1)
      }
    }
  }
}
