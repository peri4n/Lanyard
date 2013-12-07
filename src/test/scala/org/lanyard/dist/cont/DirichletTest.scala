package org.lanyard.dist.cont

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DirichletTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import DirichletTest._
  import org.lanyard.random.KISS
  import org.lanyard.random.KISSTest._

  describe("The dirichlet distribution.") {

    it("samples should always sum to one.") {
      forAll( kiss, dirichlets ) { (rng: KISS, dirichlet: Dirichlet) => 
          val samples = dirichlet.randoms(rng).take( 10000 )
          samples.foreach{ _.sum should equal( 1.0 +- 1E-15) 
        }
      }
    }
  }
}

object DirichletTest {

  val dirichlets = for {
    alphas <- Gen.containerOfN[Array, Double]( 4, Gen.choose[Double](0, 1000) )
  } yield Dirichlet( alphas )

}
