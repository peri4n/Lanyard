package org.lanyard.dist.cont

import java.io.PrintWriter
import org.lanyard.random.Ranq1
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class NormalDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The normal distribution") {
    it("can be sampled") {
      val rng = Ranq1(42)
      val samples = NormalDist(0,1).randoms(rng).take(10000)

     val writer = new PrintWriter("/home/fabian/normal", "UTF-8")
      samples.foreach( writer.println )
      writer.close
    }
  }
}










