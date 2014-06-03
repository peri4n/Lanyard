package org.lanyard.dist.disc

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class BinomialTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import BinomialTest._
  import org.lanyard.random.KISS
  import org.lanyard.random.KISSTest._

  describe("The binomial distribution") {

    it("samples should be positive and smaller or equal than n.") {
      forAll( kiss, binomials ) { ( rng: KISS, binomial: Binomial ) =>
        binomial.randoms(rng).take(10000).foreach{ _ should ( be >= 0 and be <= binomial.n ) }
      }
    }
  }
}

object BinomialTest {

  import org.lanyard.dist.cont.Beta
  import org.scalacheck.Gen

  val binomials = for {
    n <- Gen.choose(0, 1E6.toInt)
    p <- Gen.chooseNum(0.0, 1.0, 0.0, 1.0)
  } yield Binomial( n, p )

}
