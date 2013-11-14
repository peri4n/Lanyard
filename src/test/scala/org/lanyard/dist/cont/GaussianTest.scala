package org.lanyard.dist.cont

import org.lanyard.random.KISS
import org.lanyard.desc.OnlineMoments
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class NormalDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The normal distribution") {

    val DrawsPerTest = 100000

    it("can be sampled") {
      forAll( 
        (Gen.choose(Long.MinValue, Long.MaxValue), "seed"),
        (Gen.choose(1000.0, 1000.0), "mean"),
        (Gen.choose(0.0, 100.0), "variance")) {
        ( seed: Long, mean: Double, variance: Double ) => {
          val rng = KISS(seed)
          val samples = Gaussian(mean,variance).randoms(rng).take(DrawsPerTest)
          val mom = samples.foldLeft(OnlineMoments()){ (acc, x) => acc :+ x }
          mom.count should be(DrawsPerTest)
//          mom.average should be(mean plusOrMinus 0.2)
//          mom.variance should be(variance plusOrMinus 1.5)
        }
      }
    }
  }
}










