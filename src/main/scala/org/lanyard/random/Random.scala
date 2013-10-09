package org.lanyard.random

import scala.annotation.tailrec

trait Random[A] {

  def random( source: RNG ): (A, RNG)

  def randoms( source: RNG): Stream[A] = {
    val (draw, rng) = random(source)
    draw #:: randoms(rng)
  }

}

object Random {

  implicit val longRandom = new Random[Long] {
    def random( source: RNG): (Long, RNG) = source.nextLong
  }

  implicit val intRandom = new Random[Int] {
    def random( source: RNG): (Int, RNG) = source.nextInt
  }

  implicit val doubleRandom = new Random[Double] {
    def random( source: RNG): (Double, RNG) = source.nextDouble
  }

  implicit val boolRandom = new Random[Boolean] {
    def random( source: RNG): (Boolean, RNG) = source.nextBoolean
  }

}
