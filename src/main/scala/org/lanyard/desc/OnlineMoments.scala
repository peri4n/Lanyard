package org.lanyard.desc

/** Online computation of the first four moments.
  * 
  * @constructor Creates an object storing the moments
  */
case class OnlineMoments( moment0: Long = 0, moment1: Double = 0, moment2: Double = 0, moment3: Double = 0, moment4: Double = 0) {

  def :+ (value: Double): OnlineMoments = {
    val n1 = moment0 + 1
    val n2 = moment0 * moment0
    val delta = ( moment1 - value ) / n1
    val d2 = delta * delta
    val d3 = d2 * delta
    val r1 = moment0.toDouble / n1
    OnlineMoments(
      n1,      
      moment1 - delta,      
      (moment2 + n1 * d2) * r1,      
      (moment3 + (3 * delta * moment2 + (1 - n2) * d3)) * r1,      
      (moment4 + (4 * delta * moment3 + 6 * d2 * moment2 + (1 + moment0 * n2) * d2 * d2)) * r1)
  }

  def count: Long = moment0

  def mean: Option[Double] = moment0 match {
    case 0 => None 
    case _ => Some(moment1)
  }

  def variance: Option[Double] = moment0 match {
    case 0 => None
    case 1 => None
    case _ => Some((moment2 * moment0) / (moment0 - 1))
  }

  def skewness: Option[Double] = moment0 match {
    case x if x < 3 => None 
    case _ => Some(moment3 * moment0 * moment0 / (math.sqrt(variance.get) * variance.get * ( moment0 - 1) * (moment0 - 2)))
  }

  def kurtosis: Option[Double] = moment0 match {
    case x if x < 4 => None
    case _ => {
      val kFact = (moment0 - 2) * (moment0 - 3)
      val n1 = moment0 - 1
      Some( (moment4 * moment0 * moment0 * (moment0 + 1) / (variance.get * variance.get * n1) - n1 * n1 * 3) / kFact)
    }
  }
}
