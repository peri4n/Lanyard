package org.lanyard.dist.cont

import java.io.PrintWriter
import org.lanyard.random.KISS
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class GammaDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The gamma distribution") {
    ignore("can be sampled.") {
      val rng = KISS()
      val dist = new GammaDist(2, 1)
      val samples = dist.randoms(rng).take(10000)

      val writer = new PrintWriter("/home/fabian/gamma", "UTF-8")
      samples.foreach( writer.println )
      writer.close
      
    }
  }

}
