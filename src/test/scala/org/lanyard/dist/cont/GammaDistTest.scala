package org.lanyard.dist.cont

import org.lanyard.desc.Moments
import org.lanyard.random.KISS
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The gamma distribution") {

    val DrawsPerTest = 1000000

    ignore("can be sampled.") {
      forAll( 
        (Gen.choose(Long.MinValue, Long.MaxValue), "seed"),
        (Gen.choose(Double.MinPositiveValue, 100.0), "shape"),
        (Gen.choose(Double.MinPositiveValue, 100.0), "scale")) { ( seed: Long, shape: Double, scale: Double ) => {
          val rng = KISS(seed)
          val dist = new GammaDist(shape, scale)
          val samples = dist.randoms(rng).take(DrawsPerTest)
          val mom = samples.foldLeft(Moments()){ (acc, x) => acc :+ x }

//          mom.average should be( dist.mean plusOrMinus 0.5 )
//          mom.variance should be( dist.variance plusOrMinus 1.5 )
        }
      }
    }
  }
}
