package org.lanyard.dist

import org.lanyard.Measure
import org.lanyard.random.Random

/** Probability distribution of a given type `A`. It assigns a normalized measure to a
  * value of type `A` if it is in the domain. If the distribution is undefined for a given
  * value, it assigns probability zero. Every distribution is also capable of sampling.
  * 
  * @tparam A type of the distributed value
  */
trait Distribution[A] extends Measure[A] with Random[A] {

  /** Type of the parameters of the model */
  type Parameter 

}














