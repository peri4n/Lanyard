package org.lanyard

import scala.language.higherKinds

trait MeasureFac[M <: Measure[_]] {

  def create( param: M#Parameter): M

}

object MeasureFac {

  @inline def apply[M <: Measure[_]]( implicit ev: MeasureFac[M] ): MeasureFac[M] = ev

}
