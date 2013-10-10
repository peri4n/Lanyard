package org.lanyard.desc

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class MomentsTest extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val ElemLowerLimit = -1E5

  val ElemUpperLimit = 1E5

  val MeanAccuracy = 1E-8

  val VarianceAccuracy = 1E-4

  val SkewnessAccuracy = 1E-5

  val KurtosisAccuracy = 10 // BUG in scalacheck

  def mom( seq: Seq[Double], p: Int, around: Double) = {
    seq.map( x => math.pow( x - around, p) ).sum / seq.length
  }

  describe("Statistical moments") {

    it("Mean") {

      def mean( seq: Seq[Double]): Double = seq.sum / seq.length

      info("The empty list has no mean")
      val avg = List.empty[Double].foldLeft( Moments() ){ (mom, x) => mom :+ x }.average
      avg should be(None)

      info("A singleton list has the mean as elemnt.")
      forAll((Gen.choose(ElemLowerLimit, ElemUpperLimit), "element")) { 
        elem: Double => {
          val avg = List(elem).foldLeft( Moments() ) { (acc,x) => acc :+ x }.average
          avg should be(Some(elem))
        }
      }

      info("Longer lists yield an accurate result.")
      forAll((Gen.listOf1( Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] =>
          val avg = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.average
          mean(seq) should be(avg.get plusOrMinus MeanAccuracy )
      }
    }

    it("Variance") {

      def variance( seq: Seq[Double]): Double = if( seq.length == 1 ) {
        0.0
      } else {
        val avg = mom(seq, 1, 0)
        seq.map( x => math.pow( x - avg, 2) ).sum / (seq.length - 1)
      }

      info("The empty list and singleton lists have no variance.")
      val va = List.empty[Double].foldLeft( Moments() ){ (mom, x) => mom :+ x }.variance // empty list
      va should be(None)

      forAll( (Gen.choose(ElemLowerLimit, ElemUpperLimit), "element")) { 
        elem: Double => {
          val va = List(elem).foldLeft( Moments() ) { (acc,x) => acc :+ x }.variance // singleton list
          va should be(None)
        }
      }

      info("Lists with more than one element yield an accurate result.")
      forAll((Gen.listOfN( 10000, Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val va = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.variance
          variance(seq) should be(va.get plusOrMinus VarianceAccuracy)
        }
      }
    }

    it("Skewness") {

      def skewn( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        val num = mom(seq, 3, avg)
        val denom = math.pow(mom(seq, 2, avg), 1.5)
        num / denom
      }

      info("The empty list and singleton lists have no skewness.")
      val skew = List.empty[Double].foldLeft( Moments() ){ (mom, x) => mom :+ x }.skewness // empty list
      skew should be(None)

      forAll( (Gen.choose(ElemLowerLimit, ElemUpperLimit), "element")) { 
        elem: Double => {
          val skew = List(elem).foldLeft( Moments() ) { (acc,x) => acc :+ x }.skewness // singleton list
          skew should be(None)
        }
      }

      info("Lists with more than one element yield an accurate result.")
      forAll((Gen.listOfN( 10000, Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val skew = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.skewness
          skewn(seq) should be(skew.get plusOrMinus SkewnessAccuracy)
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

      info("The empty list and singleton lists have no kurtosis.")
      val kurt = List.empty[Double].foldLeft( Moments() ){ (mom, x) => mom :+ x }.kurtosis // empty list
      kurt should be(None)

      forAll( (Gen.choose(ElemLowerLimit, ElemUpperLimit), "element")) { 
        elem: Double => {
          val kurt = List(elem).foldLeft( Moments() ) { (acc,x) => acc :+ x }.kurtosis // singleton list
          kurt should be(None)
        }
      }

      info("Lists with more than one element yield an accurate result.")
      forAll((Gen.listOfN( 10000, Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence")) {
        seq: List[Double] => {
          val kurt = seq.foldLeft( Moments() ) { (acc,x) => acc :+ x }.kurtosis
          kurtosis(seq) should be(kurt.get plusOrMinus KurtosisAccuracy)
        }
      }
    }
  }
}
