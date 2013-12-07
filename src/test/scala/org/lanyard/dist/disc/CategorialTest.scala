package org.lanyard.dist.disc

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CategorialTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe( "The categorial distribution" ) {

    it( "samples only the elements of the given element." ) {

    }
  }
}

object CategorialTest {

  private def normalize( weights: Array[Double] ): Unit = {
    val sum = weights.sum
    for ( i <- 0 to weights.length ) {
      weights( i ) /= sum
    }
  }
}
