package org.lanyard.util

import org.scalacheck.Gen

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class IncGammaTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The Incomplete Gamma function") {

    it("is zero if the argument is zero.") {
      forAll( (Gen.choose(0.0, 1000.0), "a") ) { (a: Double) =>
        IncGamma(a, 0.0) should be (0.0)
      }
    }

    it("computes the same values as octave for some hand-chosen values.") {

      info("values which lead to quadrature evaluation (a >= 100).") 
      IncGamma(100.0, 2.0) / 1.87538911398785e-129 should be( 1.0 +- 1E-11 )
      IncGamma(100.0, 20.0) / 3.48887866968969e-37 should be( 1.0 +- 1E-13 )
      IncGamma(100.0, 100.0) / 0.513298798279158 should be( 1.0 +- 1E-14 )

      info("values which lead to series evaluation. (value < a + 1.0) ")
      IncGamma(5.0, 5.0) /  0.559506714934788 should be(1.0 +- 1E-14)
      IncGamma(50.0, 10.0) / 1.85472688386983E-19 should be(1.0 +- 1E-13)
      IncGamma(80, 50.0) / 5.66503552864712e-05 should be(1.0 +- 1E-13)

      info("values which lead to evaluation of the continuous fraction (else).") 
      IncGamma(1.0, 2.0) / 0.864664716763387 should be(1.0 +- 1E-13)
      IncGamma(10.0, 15.0) / 0.930146339300590 should be(1.0 +- 1E-13)
      IncGamma(60.0, 80.0) / 0.991402462741969 should be(1.0 +- 1E-13)

    }
  }
}
