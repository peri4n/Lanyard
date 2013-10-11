package org.lanyard.desc

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class MomentsTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val ElemLowerLimit = -1E2

  val ElemUpperLimit = 1E2

  val MeanAccuracy = 1E-8

  val VarianceAccuracy = 10

  val SkewnessAccuracy = 10

  val KurtosisAccuracy = 15 // BUG in scalacheck

  def mom( seq: Seq[Double], p: Int, around: Double) = {
    seq.map( x => math.pow( x - around, p) ).sum / seq.length
  }

  describe("Statistical moments") {

    it("Mean") {

      def mean( seq: Seq[Double]): Double = seq.sum / seq.length
      
      forAll((
          Gen.listOf( Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val avg = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.average
          seq match {
            case List() => avg should be(None) // The empty list has no mean
            case xs => mean(xs) should be(avg.get plusOrMinus MeanAccuracy ) // Longer lists yield an accurate result.
          }
        }
      }
    }

    it("Variance") {

      def variance( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        seq.map( x => math.pow( x - avg, 2) ).sum / (seq.length - 1)
      }

      forAll((
        Gen.listOf(
          Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val va = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.variance
          seq match {
            case xs if xs.length < 2 => va should be(None) // The empty and singleton lists have also no variance
            case xs => variance(xs) should be(va.get plusOrMinus VarianceAccuracy) // Longer lists yield an accurate result
          }
        }
      }
    }

    it("Skewness") {

      def skewness( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        val num = mom(seq, 3, avg)
        val denom = math.pow(mom(seq, 2, avg), 1.5)
        num / denom
      }

      forAll((
        Gen.listOf(
          Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val skew = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.skewness
          seq match {
            case xs if xs.length < 3 => skew should be(None)
            case xs => skewness(xs) should be(skew.get plusOrMinus SkewnessAccuracy)
          }
        }
      }
    }

    it("Kurtosis") {

      def kurtosis( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        val num = mom(seq, 4, avg)
        val denom = math.pow( mom(seq, 2, avg), 2)
        num / denom - 3
      }

      forAll((
        Gen.listOf( 
          Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val kurt = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.kurtosis
          seq match {
            case xs if xs.length < 4 => kurt should be(None)
            case xs => kurtosis(xs) should be(kurt.get plusOrMinus KurtosisAccuracy)
          }
        }
      }
    }
  }
}
