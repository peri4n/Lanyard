package org.lanyard.inference

import java.io.PrintWriter
import org.lanyard.dist.cont.Beta
import org.lanyard.dist.cont.Uniform
import org.lanyard.random.KISS

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class MetropolisHastingsTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The metropolis hastings algorithm") {

    it("can be used to sample from a beta distribution") {

      val beta = Beta(2, 5)
      val unif = Uniform(0, 1)

      val metro = MetropolisHastings[Double]( _ => unif, beta)
      val stream = metro.stream(0.5, KISS(92384576349L) )

      val writer = new PrintWriter("/home/fabian/test", "UTF-8")
      stream.take(100000).foreach( state => writer.println( state.value ) )
      writer.close

    }
  }
}
