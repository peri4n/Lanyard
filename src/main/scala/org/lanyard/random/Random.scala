package org.lanyard.random

import scala.annotation.tailrec

/** A random variable of type `A` */
trait Random[A] {
  outer =>

  /** Draws a random variable.
    * 
    * @param source source of randomness
    * @return draw and the updated RNG
    */
  def random( source: RNG ): (A, RNG)

  /** Computes an infinite stream of random draws.
    * 
    * @param source source of randomness
    * @return infinite stream of random draws.
    */
  def randoms( source: RNG): Stream[A] = {
    val (draw, rng) = random(source)
    draw #:: randoms(rng)
  }

  /** Applies a function on the random variable to yield a random variable of another type.
    * 
    * @param f function to apply on the random variable
    * @return random variable of type `B`
    */
  def map[B]( f: A => B ): Random[B] = new Random[B] {
    def random( source: RNG ): (B, RNG) = {
      val (draw, rng) = outer.random( source )
      ( f( draw ), rng )
    }
  }

  /** Applies a function on the random variable.
    * 
    * @param f function to apply on the random variable
    * @return random variable of type `B`
    */
  def flatMap[B]( f: A => Random[B] ): Random[B] = new Random[B] {
    def random( source: RNG ): (B, RNG) = {
      val (draw, rng) = outer.random( source )
      val rnd = f( draw )
      rnd.random( rng )
    }
  }
}

object Random {

  /** Random variable that can only take one value. */
  def always[A]( elem: A): Random[A] = new Random[A] {
    def random( source: RNG ) = (elem, source)
  }

  /** Random variable that can take any integer value. */
  implicit val integer: Random[Int] = new Random[Int] {
    def random( source: RNG ) = source.nextInt
  }

  /** Random variable that can take integer values in a given interval. */
  def choose[A]( low: A, high: A)(implicit scale: Scale[A], rnd: Random[A]) = {
    for { x <- rnd } yield scale(low, high, x)
  }

  /** Random variable that can take any double value in the range (0,1). */
  val double: Random[Double] = new Random[Double] {
    def random( source: RNG ) = source.nextDouble
  }

  /** Random variable that can take any `Long` value. */
  val long: Random[Long] = new Random[Long] {
    def random( source: RNG ) = source.nextLong
  }

  /** Random variable that can take either true or false. */
  val boolean: Random[Boolean] = for( x <- integer ) yield x > 0

  /** Random variable that can take on of several states. */
  def oneOf[A]( states: A*): Random[A] = 
    for {
      idx <- choose(0, states.length - 1)
    } yield states(idx)


  /** Random variable that is the tuple of two random variables. */
  def pair[A, B]( r1: Random[A], r2: Random[B] ): Random[(A, B)] = 
    for {
      first <- r1
      second <- r2
    } yield (first, second)

  /** Random variable that is the triple of three random variables. */
  def triple[A,B,C]( r1: Random[A], r2: Random[B], r3: Random[C]): Random[(A,B,C)] = 
    for {
      first <- r1
      second <- r2
      third <- r3
    } yield (first, second, third)

  trait Scale[A] {
    def apply(low: A, high: A, value: A): A 
  }

  implicit object ScaleInt extends Scale[Int] {
    def apply( low: Int, high: Int, value: Int): Int = low + value % (high - low)
  }

  implicit object ScaleDouble extends Scale[Double] {
    def apply( low: Double, high: Double, value: Double): Double = low + value / (high - low)
  }

}
