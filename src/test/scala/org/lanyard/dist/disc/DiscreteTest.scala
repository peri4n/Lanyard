package org.lanyard.dist.disc

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class DiscreteTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe( "The class for discrete types" ) {

    sealed trait DNA
    case object A extends DNA
    case object C extends DNA
    case object G extends DNA
    case object T extends DNA
    val dna = Discrete( A, C, G, T )

    it( "can be constructed with variable size." ) {
      dna.size should be( 4 )
    }

    it( "can be used to convert to integer." ) {
      dna.asInt( A ) should be( 0 )
      dna.asInt( C ) should be( 1 )
      dna.asInt( G ) should be( 2 )
      dna.asInt( T ) should be( 3 )
    }

    it( "can be used to convert from integer." ) {
      dna.fromInt( 0 ) should be ( A )
      dna.fromInt( 1 ) should be ( C )
      dna.fromInt( 2 ) should be ( G )
      dna.fromInt( 3 ) should be ( T )
    }
  }
}
