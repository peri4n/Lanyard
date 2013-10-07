package org.lanyard.random

import java.io.PrintWriter
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class KISSTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  describe("The KISS RNG") {
    it("should draw double in the range of (0,1).") {
      val rng = KISS()

      def draws( source: RNG ): Stream[Double] = {
        val (d, rng) = source.nextDouble
        d #:: draws(rng)
      }

      val samples = draws(rng).take(1000000)
      samples.forall( s => 0 < s && s < 1) should be(true)

      // val writer = new PrintWriter("/home/fabian/kiss", "UTF-8")
      // samples.foreach( writer.println )
      // writer.close
    }
  }
}
