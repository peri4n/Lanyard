package org.lanyard.dist.cont

import org.lanyard.random.Ranq1
import org.lanyard.desc.Moments
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
          val rng = Ranq1(seed)
          val samples = NormalDist(mean,variance).randoms(rng).take(DrawsPerTest)
          val mom = samples.foldLeft(Moments()){ (acc, x) => acc :+ x }
          mom.count should be(DrawsPerTest)
          mom.average should be(mean plusOrMinus 0.1)
          mom.variance should be(variance plusOrMinus 1)
        }
      }
    }
  }
}










