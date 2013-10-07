package org.lanyard.random

import scala.annotation.tailrec

trait Random[A] {

  def random( source: RNG ): (A, RNG)

  def randoms( source: RNG): Stream[A] = {
    val (draw, rng) = random(source)
    draw #:: randoms(rng)
  }

}
