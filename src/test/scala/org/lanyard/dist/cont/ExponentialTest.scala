package org.lanyard.dist.cont

import org.lanyard.dist.DistFactory
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ExponentialTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The exponential distribution") {

    it("is parameterized by a strictly positive floating point number (lambda).") {

      info("A negative lambda should throw an exception.")
      forAll( (Gen.negNum[Double], "lambda")) { ( lambda: Double ) =>
        intercept[IllegalArgumentException] {
          Exponential(lambda)
          DistFactory[Exponential].create(lambda)
        }
      }

      info("If lambda equals zero, throw an exception")
      intercept[IllegalArgumentException] {
        Exponential(0.0)
        DistFactory[Exponential].create(0.0)
      }

      info("A positive lambda should not throw an exception.")
      forAll( (Gen.posNum[Double], "lambda")) { ( lambda: Double ) =>
        val exp = Exponential(lambda)
        val expFac = DistFactory[Exponential].create(lambda)
        exp.lambda should be(lambda)
        expFac.lambda should be(lambda)
      }
    }
  }

}
