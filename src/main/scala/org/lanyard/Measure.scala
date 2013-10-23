package org.lanyard

/** A Model is a function that asigns a probability to a set of values of type A.
  * 
  * @tparam A Type of the values to be modeled
  */
trait Measure[A] extends (A => Double) {

  /** Type of the parameters of the model */
  type Parameter 

  def factory( implicit ev: MeasureFac[Measure[A]]) = ev

  /** Assigns an unnormalized score to a value.
    * 
    * @param value Value to assign a score to
    * @return unnormalized measure
    */
  def apply( value: A): Double

  /** Assigns a log-probability to a value of type A
    * 
    * @param value Value to compute the probability for
    * @return logarithm of the probabilty of the value
    */
  def logLike(value: A): LogLike = math.log(apply(value))

}
