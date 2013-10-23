package org.lanyard.dist.disc

import java.io.PrintWriter
import org.lanyard.random.KISS
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class BinomialDistTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The binomial distribution") {

    it("can be sampeld") {

      val rng = KISS(348236875L)
      val dist = Binomial(50, 0.3)
      val draws = dist.randoms(rng).take(100000)
      val printer = new PrintWriter("/home/fabian/test", "UTF-8")
      draws.foreach( printer.println )
      printer.close
    }
  }
}
