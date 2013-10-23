package org.lanyard.dist.cont

import org.lanyard.random.KISS
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DirichletDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The dirichlet distribution.") {

    it("can be sampled") {
      val dist = Dirichlet( Array(1.0, 1.0, 1.0, 1.0) )
      val rng = KISS(99823745987L)
      val (draw, _ ) = dist.random(rng) 
      println( draw.mkString(" ") )
      println( draw.sum )
    }

    it("computes the correct probability density function.") {
      val dist = Dirichlet( Array(1.0, 1.0, 1.0, 1.0) )
      dist( Array(0.25, 0.25, 0.25, 0.25) ) should be( math.log(6.0) plusOrMinus 1E-10 )
    }
  }
}










