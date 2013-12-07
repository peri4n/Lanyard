package org.lanyard.desc

/** Utility object to store the function that computes the moments. */
object Moments {

  import math._
  import scala.util._

  private val fail = Failure( new ArithmeticException )

  /** Computes the first four moments. 
    * 
    * It uses the two-pass algorithm from ''Bevington, P.R. and Robinson, D.K. 2002. 
    * Data Reduction and Error Analysis for the Physical Sciences. 3rd edition. McGraw-Hill. Chapter 1''
    * 
    * @param values sequence of values
    * @return A five tuple of (length, mean, variance, skewness, kurtosis)
    */
  def apply( values: Seq[Double] ): (Int, Try[Double], Try[Double], Try[Double], Try[Double]) = 
    if( values.isEmpty )
      (0, fail, fail, fail, fail )
    else {

      val (length, sum) = values.foldLeft( (0, 0.0) ) { (acc, x) => ( acc._1 + 1, acc._2 + x ) } // first pass
      val mean = sum / length

      var (p, s) = (0.0, 0.0)
      var (adev, ep, vari, skew, kurt) = (0.0, 0.0, 0.0, 0.0, 0.0 ) 
      values.foreach{ x => // second pass
        s = x - mean
        p = s * s
        adev += abs( s )
        ep += s
        vari += p
        p *= s
        skew += p
        p *= s
        kurt += p
      }
      
      adev /= length
      vari = ( vari - ep * ep / length) / ( length - 1)
      ( length,
        if( length != 0 ) Success( mean ) else fail,
        if( length > 1 ) Success( vari ) else fail,
        if( vari != 0 ) Success( skew / (length * vari * sqrt( vari )) ) else fail,
        if( vari != 0 ) Success( kurt / (length * vari * vari) - 3 ) else fail)
    }

}
