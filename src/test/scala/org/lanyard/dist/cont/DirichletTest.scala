package org.lanyard.dist.cont

import org.lanyard.random.KISS
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DirichletDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The dirichlet distribution.") {

    it("samples should always sum to one.") {
      forAll( 
        (Gen.containerOfN[Array, Double]( 4, Gen.choose[Double](0, 100) ), "alphas" ),
        (Gen.posNum[Long], "seed")
      ){
        ( alphas: Array[Double], seed: Long ) => {
          val dist = Dirichlet( alphas )
          val rng = KISS(seed)
          val (draw, _) = dist.random(rng)
          draw.sum should equal( 1.0 +- 0.001)
        }
      }
    }
  }
}
