package org.lanyard.dist.cont

import org.lanyard._
import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.util.LogGamma

class DirichletDist ( val alphas: Array[Double] ) extends Distribution[Array[Double]] {

  private val dimension = alphas.length

  private lazy val gammas = alphas.map( GammaDist( _ , 1.0) )

  private lazy val constantTerm = LogGamma( alphas.sum ) - alphas.map( LogGamma ).sum

  private def sumsToOne( values: Array[Double] ): Boolean = math.abs(values.sum - 1) < 1E-10

  def apply( value: Array[Double] ): Prob = { 
    var (i, sum) = (0, 0.0)
    while( i < dimension) {
      sum += ( alphas(i) - 1 ) * math.log(value(i))
      i += 1
    }
    constantTerm + sum
  }

  def random( source: RNG): (Array[Double], RNG) = {
    val draw = Array.ofDim[Double]( dimension )
    var (i, sum) = (0, 0.0)
    var rng = source
    while( i < dimension ) {
      val (x, nextRNG) = gammas(i).random(rng)
      draw(i) = x
      rng = nextRNG
      sum += x
      i += 1
    }
    i = 0
    while( i < dimension ) {
      draw(i) /= sum
      i += 1
    }
    assume( sumsToOne(draw), "Draw of dirichlet distribution did not sum to one.")

    (draw, rng)
  }
}










