package org.lanyard.test

/** The Chi-Square-Test */
object ChiSquareTest {

  def one( sample: Array[Double], expected: Array[Double], constraints: Int = 1 ): ChiSquareTestResult = ???

  def two( sample1: Array[Double], sample2: Array[Double], constraints: Int = 1 ): ChiSquareTestResult = ???

  case class ChiSquareTestResult( pValue: Double, df: Int, statistic: Double )

}
