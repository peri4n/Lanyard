package org.lanyard

/** A Model is a function that asigns a probability to a set of values of type A.
  * 
  * @tparam A Type of the values to be modeled
  */
trait Model[A] extends ( A => Prob ) {

  /** Type of the parameters of the model */
  type Parameter 

  def factory( implicit ev: ModelFac[Model[A]]) = ev

  /** Assigns a log-probability to a value of type A
    * 
    * @param value Value to compute the probability for
    * @return logarithm of the probabilty of the value
    */
  def probability(value: A): Prob = apply(value)

}
