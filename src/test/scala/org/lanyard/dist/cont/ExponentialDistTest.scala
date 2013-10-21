package org.lanyard.dist.cont

import org.lanyard.ModelFac
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ExponentialDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The exponential distribution") {

    it("is parameterized by a strictly positive floating point number (lambda).") {
      info("A negative lambda should throw an exception.")
      forAll( (Gen.negNum[Double], "lambda")) { ( lambda: Double ) =>
        intercept[IllegalArgumentException] {
          Exponential(lambda)
          ModelFac[Exponential].create(lambda)
        }
      }

      info("If lambda equals zero, throw an exception")
      intercept[IllegalArgumentException] {
        Exponential(0.0)
        ModelFac[Exponential].create(0.0)
      }

      info("A positive lambda should not throw an exception.")
      forAll( (Gen.posNum[Double], "lambda")) { ( lambda: Double ) =>
        Exponential(lambda)
        ModelFac[Exponential].create(lambda)
      }
    }
  }

}
