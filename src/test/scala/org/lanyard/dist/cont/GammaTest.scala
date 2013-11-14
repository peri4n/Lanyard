package org.lanyard.dist.cont

import org.lanyard.random.KISS
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The gamma distribution") {

    val DrawsPerTest = 1000

    it("samples should have the correct moments.") {
      forAll( 
        (Gen.choose(Long.MinValue, Long.MaxValue), "seed"),
        (Gen.choose(Double.MinPositiveValue, 100.0), "shape"),
        (Gen.choose(Double.MinPositiveValue, 10.0), "scale")) { ( seed: Long, shape: Double, scale: Double ) => {
          val rng = KISS(seed)
          val dist = Gamma(shape, scale)
          val samples = dist.randoms(rng).take(DrawsPerTest)

          // val writer = new PrintWriter("/home/fabian/gamma", "UTF-8")
          // samples.foreach( writer.println )
          // writer.close

          // val mom = samples.foldLeft(Moments()){ (acc, x) => acc :+ x }
          // mom.average.get should be( dist.mean plusOrMinus 0.5 )
          // mom.variance.get should be( dist.variance plusOrMinus 10 )
        }
      }
    }
  }
}
