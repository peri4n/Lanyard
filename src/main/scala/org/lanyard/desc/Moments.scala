package org.lanyard.desc

import scala.collection.immutable

object Moments {

  import math._

  /** Computes the first four moments. 
    * 
    * It uses the two-pass algorithm from ''Bevington, P.R. and Robinson, D.K. 2002. 
    * Data Reduction and Error Analysis for the Physical Sciences. 3rd edition. McGraw-Hill. Chapter 1''
    * 
    * @param values sequence of values
    * @return A five tuple of (length, mean, variance, skewness, kurtosis)
    */
  def apply( values: Seq[Double] ): (Int, Option[Double], Option[Double], Option[Double], Option[Double]) = 
    if( values.isEmpty )
      (0, None, None, None, None )
    else {

      val (length, sum) = values.foldLeft( (0, 0.0) ) { (acc, x) => ( acc._1 + 1, acc._2 + x ) }
      val mean = sum / length

      var (p, s) = (0.0, 0.0)
      var (adev, ep, vari, skew, kurt) = (0.0, 0.0, 0.0, 0.0, 0.0 ) 
      values.foreach{ x =>
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
        if( length != 0 ) Some( mean ) else None,
        if( length > 1 ) Some( vari ) else None,
        if( vari != 0 ) Some( skew / (length * vari * sqrt( vari )) ) else None,
        if( vari != 0 ) Some( kurt / (length * vari * vari) - 3 ) else None)
    }

}
