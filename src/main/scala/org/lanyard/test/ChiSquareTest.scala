package org.lanyard.test

import org.lanyard.util.IncGamma

/** The Chi-Square-Test */
object ChiSquareTest {

  /**
    * Test result of a chi square test.
    * 
    * @constructor Create a test result of a chi square test
    * @param pValue p-value of the test statistic
    * @param df degrees of freedom used
    * @param statistic test statistic
    */
  case class ChiSquareTestResult(pValue: Double, df: Int, statistic: Double)

  /**
   * Tests if a sample matches the expected distribution.
   *
   * @param sample sample to test
   * @param expected expected occurences of events
   * @param constraints constraints which decrease the degrees of freedom
   * @return test result
   */
  def one(sample: Array[Double], expected: Array[Double], constraints: Int = 1): ChiSquareTestResult = {
    require(sample.length == expected.length, "ChiSquareTest argument 'sample' and 'expected' need to have the same length.")

    val bins = sample.length

    var degFree = bins - constraints
    var chiSqu = 0.0

    var j = 0
    while (j < bins) {
      if (expected(j) < 0.0 || (expected(j) == 0.0 && sample(j) > 0.0)) throw new Exception("Bad expected occurences found.")

      if (expected(j) == 0.0 && sample(j) == 0.0) {
        degFree -= 1
      } else {
        val tmp = sample(j) - expected(j)
        chiSqu += tmp * tmp / expected(j)
      }

      j += 1
    }
    ChiSquareTestResult(IncGamma.oneMinus(0.5 * degFree, 0.5 * chiSqu), degFree, chiSqu)
  }

  /**
   * Tests if two samples come from the same distribution.
   *
   * @param sample1 first sample
   * @param sample2 second sample
   * @param constraints constraints which decrease the degrees of freedom
   * @return test result
   */
  def two(sample1: Array[Double], sample2: Array[Double], constraints: Int = 1): ChiSquareTestResult = {
    require(sample1.length == sample2.length, "ChiSquareTest argument 'sample1' and 'sample2' need to have the same length.")

    val bins1 = sample1.length
    val bins2 = sample2.length

    var degFree = bins1 - constraints
    var chiSqu = 0.0

    var j = 0
    while (j < bins1) {
      if (sample1(j) == 0.0 && sample2(j) == 0.0) {
        degFree -= 1
      } else {
        val temp = sample1(j) - sample2(j)
        chiSqu += temp * temp / (sample1(j) + sample2(j))
      }
      j += 1
    }
    ChiSquareTestResult(IncGamma.oneMinus(0.5 * degFree, 0.5 * chiSqu), degFree, chiSqu)
  }

}
