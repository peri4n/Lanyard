package org.lanyard.dist.cont

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import GammaTest._
  import org.lanyard.random.KISS
  import org.lanyard.random.KISSTest._

  describe( "The gamma distribution" ) {

    it( "samples should all be strictly positive." ) {
      forAll( kiss, gammas ) { ( rng: KISS, gamma: Gamma ) =>
        gamma.randoms( rng ).take( 10000 ).foreach{ _ should be > 0.0 }
      }
    }

    import org.lanyard.desc.Moments

    it( "samples should have approximately the correct mean." ) { 
      forAll( kiss, gammas ) { ( rng: KISS, gamma: Gamma ) =>
        val samples = gamma.randoms( rng ).take( 100000 )
        val ( _, mean, _, _, _ ) = Moments( samples )
        mean.get should be( gamma.mean +- 5 )
      }
    }

    it( "samples should have approximately the correct variance." ) {
      forAll( kiss, gammas ) { ( rng: KISS, gamma: Gamma ) =>
        val samples = gamma.randoms( rng ).take( 100000 )
        val ( _, _, variance, _, _ ) = Moments( samples )
        variance.get should be( gamma.variance +- 100 )
      }
    }
  }
}

object GammaTest {

  import org.scalacheck.Gen

  val gammas = for {
    shape <- Gen.choose( Double.MinPositiveValue, 100.0 )
    scale <- Gen.choose( Double.MinPositiveValue, 10.0 )
  } yield Gamma( shape, scale )

}
