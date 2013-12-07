package org.lanyard.dist.cont

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class UniformTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import UniformTest._
  import org.lanyard.random.KISS
  import org.lanyard.random.KISSTest._

  describe( "The continuous uniform distribution" ) {

    it( "samples should be in the range of [leftLimit,rightLimit]" ) {
      forAll( kiss, uniforms ) { ( rng: KISS, uniform: Uniform ) =>
        uniform.randoms( rng ).take( 10000 ).foreach{ _ should ( be >= uniform.leftLimit and be <= uniform.rightLimit ) }
      }
    }
  }
}

object UniformTest {

  import math._
  import org.scalacheck.Gen

  val uniforms = for {
    boundOne <- Gen.choose( -1E6, 1E6 )
    boundTwo <- Gen.choose( -1E6, 1E6 )
  } yield Uniform( min( boundOne, boundTwo ), max( boundOne, boundTwo ) )

}
