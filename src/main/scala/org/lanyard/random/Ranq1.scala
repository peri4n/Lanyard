package org.lanyard.random

class Ranq1(private val v: Long) extends RNG {

  def nextLong: (Long, Ranq1) = {
    var newV = v ^ (v >>> 21)
    newV ^= newV << 35
    newV ^= newV >>> 4
    (newV * 2685821657736338717L, new Ranq1(newV))
  }
}

object Ranq1 {

  def apply( seed: Long) = {
    var v = 4101842887655102017L ^ seed
    v ^= v >>> 21
    v ^= v << 35
    v ^= v >>> 4
    new Ranq1( v * 2685821657736338717L )
  }
}
