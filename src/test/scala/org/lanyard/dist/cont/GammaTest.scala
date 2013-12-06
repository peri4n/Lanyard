package org.lanyard.dist.cont

import java.io.FileWriter
import org.lanyard.desc.OnlineMoments
import org.lanyard.random.KISS
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The gamma distribution") {

    it("samples should all be strictly positive.") {
      forAll(
        (Gen.posNum[Long], "seed"),
        (Gen.choose(Double.MinPositiveValue, 1000.0), "shape"),
        (Gen.choose(Double.MinPositiveValue, 100.0), "scale")) {
          (seed: Long, shape: Double, scale: Double) =>
            {
              val dist = Gamma(3.5, 5)
              val samples = dist.randoms(KISS(seed)).take( 10000 )

              samples.foreach{ _ should be > 0.0 }
            }
      }
    }

    it("samples with the correct variance") { pending
      forAll(
        (Gen.posNum[Long], "seed"),
        (Gen.choose(Double.MinPositiveValue, 100.0), "shape"),
        (Gen.choose(Double.MinPositiveValue, 10.0), "scale")) {
          (seed: Long, shape: Double, scale: Double) =>
            {
              val dist = Gamma(shape, scale)
              val samples = dist.randoms(KISS(seed)).take( 100000 )
              val mom = samples.foldLeft( OnlineMoments() ){ (acc, x) => acc :+ x }
              mom.variance.get should be(dist.variance +- 50)
            }
      }
    }

    it("samples with the correct mean") { pending }
  }
}
