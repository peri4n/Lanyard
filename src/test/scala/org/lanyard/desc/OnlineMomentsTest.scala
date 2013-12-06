package org.lanyard.desc

import java.lang.ArithmeticException

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.util._

/** Tests for the online moments */
class OnlineMomentsTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  /** For some strange reason, the length of generated random sequences is linked to the accuracy constants in the tests.
    * This is most probably a bug in the testing framework. Therefore, the accuracies are a to the kept as a lower limit. */ 

  /** Upper limit of the elements drawn for random sequences */
  val ElemLowerLimit = -1E3

  /** Lower limit of the elements drawn for random sequences */
  val ElemUpperLimit = 1E3

  /** START OF TESTS */
  describe("Statistical moments") {

    /** Auxillary function used in the computation of the moments */ 
    def mom( seq: Seq[Double], p: Int, around: Double) = seq.map( x => math.pow( x - around, p) ).sum / seq.length

    it("The computation of the mean") {

      /** Accuracy to expect from the mean */
      val MeanAccuracy = 1E-12

      /** Computes the mean in a standard fashion */
      def mean( seq: Seq[Double]): Double = seq.sum / seq.length

      info( "results in a failure for an empty array." )
      val avg = Array.empty[Double].foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.mean
      avg.isFailure should be( true )

      info( "retains a sufficient accuracy." )
      forAll( (Gen.containerOfN[Array, Double]( 100, Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence") ) {
        seq: Array[Double] => {
          val avg = seq.foldLeft( OnlineMoments() ) { (acc,x) => acc :+ x }.mean
          avg.get should equal ( mean( seq ) +- MeanAccuracy )
        }
      }
    }

    it("The computation of the variance") {

      /** Accuracy to expect from the variance */
      val VarianceAccuracy = 1E-9

      /** Computes the variance in a standard fashion */
      def variance( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        seq.map( x => math.pow( x - avg, 2) ).sum / (seq.length - 1)
      }

      info( "results in a failure for an empty or singleton array." )
      val va = Array.empty[Double].foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.variance
      va.isFailure should be( true )

      forAll { 
        elem: Double => {
          val va = Array( elem ).foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.variance
          va.isFailure should be( true )
        }
      }

      info( "retains a sufficient accuracy" )
      forAll( (Gen.containerOfN[Array, Double]( 100, Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence") ) {
        seq: Array[Double] => {
          val va = seq.foldLeft( OnlineMoments() ) { (acc,x) => acc :+ x }.variance
          va.get should equal ( variance( seq ) +- VarianceAccuracy)
        }
      }
    }

    it("The computation of the skewness") {

      /** Accuracy to expect from the skewness */
      val SkewnessAccuracy = 1E-2

      /** Computes the skewness in a standard fashion */
      def skewness( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        val num = mom(seq, 3, avg)
        val denom = math.pow(mom(seq, 2, avg), 1.5)
        num / denom
      }

      info( "results in a failure for arrays with less than three elements." )
      val skew = Array.empty[Double].foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.skewness
      skew.isFailure should be( true )

      forAll { 
        pair: (Double, Double) => {
          val skew = Array(pair._1, pair._2).foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.skewness
          skew.isFailure should be( true )
        }
      }

      info( "retains a sufficient accuracy" )
      forAll( (Gen.containerOfN[Array, Double]( 100, Gen.choose(ElemLowerLimit, ElemUpperLimit)), "sequence") ) {
        seq: Array[Double] => {
          val skew = seq.foldLeft( OnlineMoments() ) { (acc,x) => acc :+ x }.skewness
          skew.get should equal( skewness(seq) +- SkewnessAccuracy )
        }
      }
    }

    it("The computaion of the kurtosis") {

      /** Accuracy to expect from the kurtosis */
      val KurtosisAccuracy = 3E-2

      /** Computes the kurtosis in a standard fashion */
      def kurtosis( seq: Seq[Double]): Double = {
        val avg = mom(seq, 1, 0)
        val num = mom(seq, 4, avg)
        val denom = math.pow( mom(seq, 2, avg), 2)
        num / denom - 3
      }

      info( "results in a failure for arrays with less than four elements." )
      val kurt = Array.empty[Double].foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.kurtosis
      kurt.isFailure should be( true )

      forAll { 
        triple: (Double, Double, Double) => {
          val kurt = Array(triple._1, triple._2, triple._3 ).foldLeft( OnlineMoments() ) { (acc, x ) => acc :+ x }.kurtosis
          kurt.isFailure should equal( true )
        }
      }

      info( "retains a sufficient accuracy" )
      forAll(Gen.containerOfN[Array, Double]( 100, Gen.choose(ElemLowerLimit, ElemUpperLimit) )) {
        seq: Array[Double] => {
          val kurt = seq.foldLeft( OnlineMoments() ) { (acc,x) => acc :+ x }.kurtosis
          kurt.get should equal( kurtosis(seq) +- KurtosisAccuracy )
        }
      }
    }
  }
}
