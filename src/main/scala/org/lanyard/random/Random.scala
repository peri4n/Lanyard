package org.lanyard.random

import scala.annotation.tailrec

trait Random[A] {
  outer =>

  def random( source: RNG ): (A, RNG)

  def randoms( source: RNG): Stream[A] = {
    val (draw, rng) = random(source)
    draw #:: randoms(rng)
  }

  def map[B]( f: A => B ): Random[B] = new Random[B] {
    def random( source: RNG ): (B, RNG) = {
      val (draw, rng) = outer.random( source )
      ( f( draw ), rng )
    }
  }

  def flatMap[B]( f: A => Random[B] ): Random[B] = new Random[B] {
    def random( source: RNG ): (B, RNG) = {
      val (draw, rng) = outer.random( source )
      val rnd = f( draw )
      rnd.random( rng )
    }
  }
}

object Random {

  def integer: Random[Int] = new Random[Int] {
    def random( source: RNG) = source.nextInt
  }

  def boolean: Random[Boolean] = for( x <- integer ) yield x > 0

  def pair[A, B]( r1: Random[A], r2: Random[B] ): Random[(A, B)] = for {
    first <- r1
    second <- r2
  } yield (first, second)

}
