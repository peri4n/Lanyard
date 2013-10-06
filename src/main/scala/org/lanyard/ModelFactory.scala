package org.lanyard

import scala.language.higherKinds

trait ModelFac[M <: Model[_]] {

  def create( param: M#Parameter): M

}
