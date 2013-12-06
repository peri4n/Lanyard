package org.lanyard.inference

import org.lanyard.dist.Distribution

trait ML[A, D <: Distribution[A] ] {

  def train( samples: List[A] ): D

}
