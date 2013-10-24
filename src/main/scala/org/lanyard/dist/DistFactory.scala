package org.lanyard.dist

import scala.language.higherKinds

/** Higher kinded type to create arbitrary distributions. */
trait DistFactory[D <: Distribution[_]] {

  def create( param: D#Parameter): D

}

object DistFactory {

  @inline def apply[D <: Distribution[_]]( implicit ev: DistFactory[D] ): DistFactory[D] = ev

}
