package org.lanyard.dist.cont

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class BetaTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import BetaTest._
  import org.lanyard.random.KISS
  import org.lanyard.random.KISSTest._

  describe("The beta distribution") {

    it("samples should all be in the interval [0,1]") {
      forAll( kiss, betas) { (rng: KISS, beta: Beta) =>
        beta.randoms( rng ).take( 10000 ).foreach { _ should (be >= 0.0 and be <= 1.0) }
      }
    }
  }

}

object BetaTest {

  val betas = for {
    alpha <- Gen.posNum[Double]
    beta <- Gen.posNum[Double]
  } yield Beta( alpha, beta)

}

