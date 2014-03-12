package org.lanyard.dist.cont

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

/** The dirichlet distribution is a continuous probability
  * distribution. It assigns probabilities to vectors in the simplex
  * of some dimension.
  * 
  * @constructor Creates a dirichlet distribution with the given parameters.
  * @param alpha alpha parameter of the dirichlet distribution
  */
class Dirichlet private( private val alpha: Array[Double] ) extends Distribution[Array[Double]] {

  import math._
  import org.lanyard.util.LogGamma

  require( alpha.forall( _ > 0.0 ), 
    "Dirichlet distribution parameter alpha has to have strictly positive elements. Found value: " + alpha.mkString(" ") )

  /** Dimension of the simplex for which the dirichlet distribution is defined. */
  val dimension = alpha.length

  /** Gamma distributions needed for sampling */
  private val gammas = alpha.map( Gamma( _ , 1.0) )

  /** Log of the constant term of the dirichlet distribution. */
  private val constantLogTerm = LogGamma( alpha.sum ) - alpha.map( LogGamma ).sum

  /** Checks if an array is in the domain of the dirichlet distribution.
    * 
    * @param values presumably statistical vector
    * @return true if values sums to one, false otherwise
    */
  private def isInDomain( values: Array[Double] ): Boolean = math.abs(values.sum - 1) < 1E-10 && values.length == alpha.length

  /** Computes the mean. */
  def mean: Array[Double] = {
    val alphaSum = alpha.sum
    alpha.map( _ / alphaSum )
  }

  /** Computes the variance. */
  def variance: Array[Double] = {
    val alphaSum = alpha.sum
    alpha.map( a => ( a * (alphaSum - a)) / ( alphaSum * alphaSum * (alphaSum + 1)))
  }

  /** Computes the log probability density function.
    * 
    * @param value presumably statistical vector
    * @return value of the log pdf
    */
  override def logLike( value: Array[Double] ): LogLike = 
    if( isInDomain( value ) ) { 
      var (i, sum) = (0, 0.0)
      while( i < dimension) {
        sum += ( alpha(i) - 1 ) * math.log(value(i))
        i += 1
      }
      constantLogTerm + sum
    } else { // dirichlet is not defined at value
      Double.NegativeInfinity
    }

  /** Draws a random number from this dirichlet distribution.
    * The algorithm normalizes [[Gamma]] draws.
    * 
    * @param source source of randomness
    * @return pair of the draw and the updated RNG
    */
  def random( source: RNG): (Array[Double], RNG) = {
    val draw = Array.ofDim[Double]( dimension )
    var (i, sum) = (0, 0.0)
    var rng = source
    while( i < dimension ) { 
      val (x, nextRNG) = gammas(i).random(rng) // draw a gamma in each dimension
      draw(i) = x
      rng = nextRNG
      sum += x
      i += 1
    }
    i = 0
    while( i < dimension ) { // normalize draws
      draw(i) /= sum
      i += 1
    }
    assume( isInDomain(draw), "Draw of dirichlet distribution did not sum to one. Parameter alpha: [" + alpha.mkString(" ") + "]" )

    (draw, rng)
  }
}

object Dirichlet {

  /** Create a dirichlet distribution with given alpha.
    * 
    * @param alpha alpha parameter of the dirichlet distribution
    * @return dirichlet distribution with given alpha
    */
  def apply( alpha: Seq[Double] ): Dirichlet = new Dirichlet( alpha.toArray )

}









