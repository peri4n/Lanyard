package org.lanyard.util

import org.scalacheck.Gen

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class LogGammaTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import math._

  describe("The logarithm of the gamma function.") {

    it("computes the same values as octave for some hand-chosen values.") {
      LogGamma(1.000000000000) should be(0.0)
      LogGamma(4.237648658534) should be(2.09811555772708 +- 1E-14)
      LogGamma(8.593874597983) should be(9.74508287725600 +- 1E-14)
      LogGamma(2378.9823475938) should be(16113.2766010997 +- 1E-10)
    }

    it("computes the correct recursion.") {
      forAll((Gen.choose(0.0, 1000.0), "argument")) { (value: Double) =>
        LogGamma(value + 1) should be(LogGamma(value) + log(value) +- 1E-11)
      }
    }
  }
}
