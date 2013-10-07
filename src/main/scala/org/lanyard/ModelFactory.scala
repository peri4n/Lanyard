package org.lanyard

import scala.language.higherKinds

trait ModelFac[M <: Model[_]] {

  def create( param: M#Parameter): M

}

object ModelFac {

  @inline def apply[M <: Model[_]]( implicit ev: ModelFac[M] ): ModelFac[M] = ev

}
