package org.lanyard.dist.cont

import java.io.PrintWriter
import org.lanyard.desc.Moments
import org.lanyard.random.KISS
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The gamma distribution") {

    val DrawsPerTest = 1000000

    it("samples should have the correct moments.") {
      forAll( 
        (Gen.choose(Long.MinValue, Long.MaxValue), "seed"),
        (Gen.choose(Double.MinPositiveValue, 100.0), "shape"),
        (Gen.choose(Double.MinPositiveValue, 10.0), "scale")) { ( seed: Long, shape: Double, scale: Double ) => {
          val rng = KISS(seed)
          val dist = GammaDist(shape, scale)
          val samples = dist.randoms(rng).take(DrawsPerTest)

          val writer = new PrintWriter("/home/fabian/gamma", "UTF-8")
          samples.foreach( writer.println )
          writer.close

          val mom = samples.foldLeft(Moments()){ (acc, x) => acc :+ x }
          mom.average.get should be( dist.mean plusOrMinus 0.5 )
          mom.variance.get should be( dist.variance plusOrMinus 10 )
        }
      }
    }
  }
}
